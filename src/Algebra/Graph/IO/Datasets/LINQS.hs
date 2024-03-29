{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# language DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
-- | Link-based datasets from https://linqs.soe.ucsc.edu/data
{-# options_ghc -Wno-unused-imports -Wno-unused-top-binds #-}
module Algebra.Graph.IO.Datasets.LINQS (
  restoreContent, CitesRow(..), ContentRow(..), 
  -- * Internal
  stash,
  sourceGraphEdges, loadGraph
                                       ) where

import Control.Applicative (Alternative(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Functor (($>), void)

import GHC.Generics (Generic(..))
import GHC.Int (Int16)

-- algebraic-graphs
import qualified Algebra.Graph as G (Graph, empty, overlay, edge)
-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
-- binary
import Data.Binary (Binary(..), encode, decode, encodeFile, decodeFileOrFail)
-- binary-conduit
import qualified Data.Conduit.Serialization.Binary as CB (conduitDecode, conduitEncode, ParseError(..))
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield, await, runConduitRes)
import qualified Data.Conduit.Combinators as C (print, sourceFile, sinkFile, map, mapM, foldM, mapWhile, mapAccumWhile, foldMap, foldl, scanl)
-- containers
import Data.Sequence (Seq, (|>))
import qualified Data.Map as M (Map, singleton, lookup)
-- directory
import System.Directory (createDirectoryIfMissing)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- filepath
import System.FilePath ((</>), takeFileName, takeExtension)
-- http-conduit
import Network.HTTP.Simple (httpSource, getResponseBody, Response, Request, parseRequest, setRequestMethod)
-- megaparsec
import Text.Megaparsec (parse, runParserT)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Error (errorBundlePretty, ParseErrorBundle)
-- parser.combinators
import Control.Monad.Combinators (count)
-- tar-conduit
import Data.Conduit.Tar (Header(..), untarChunks, TarChunk, withEntries, FileInfo, filePath, withFileInfo, headerFileType, FileType(..), headerFilePath)
-- text
import qualified Data.Text as T (Text, unwords)



import Algebra.Graph.IO.Internal (fetch, unTarGz, Parser, ParserT, ParseE, symbol, lexeme, alphaNum)
import Algebra.Graph.IO.SV (parseTSV)

{-
CiteSeer: The CiteSeer dataset consists of 3312 scientific publications classified into one of six classes. The citation network consists of 4732 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 3703 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz

Cora: The Cora dataset consists of 2708 scientific publications classified into one of seven classes. The citation network consists of 5429 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 1433 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/cora.tgz

WebKB: The WebKB dataset consists of 877 scientific publications classified into one of five classes. The citation network consists of 1608 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 1703 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/WebKB.tgz

-}


-- | Download, decompress, parse, serialize and save the dataset to local storage
stash :: (Binary c) =>
         FilePath -- ^ directory where the data files will be saved
      -> String -- ^ URI of .tar.gz file
      -> Int -- ^ dictionary size
      -> Parser c -- ^ document class
      -> IO ()
stash dir uri n pc = do
  rq <- parseRequest uri
  createDirectoryIfMissing True dir
  runResourceT $ runConduit $
    fetch rq .|
    unTarGz .|
    withFileInfo ( \fi -> do
     contentToFile dir n pc fi
     citesToFile dir fi )

-- | Load the graph node data from local storage
restoreContent :: (Binary c) => FilePath -- ^ directory where the data files are saved
               -> IO (M.Map String (Int16, Seq Int16, c))
restoreContent dir = runResourceT $ runConduit $
  contentFromFile dir .|
  C.foldMap ( \(CRow i k fs c) -> M.singleton k (i, fs, c) )


citesFromFile :: (MonadResource m, MonadThrow m) => FilePath -> ConduitT i (CitesRow String) m ()
citesFromFile dir =
  C.sourceFile (dir </> "cites") .|
  CB.conduitDecode

-- | Reconstruct the citation graph
--
-- NB : relies on the user having `stash`ed the dataset to local disk first.
loadGraph :: (Binary c) =>
             FilePath -- ^ directory where the data files were saved
          -> IO (G.Graph (ContentRow Int16 c))
loadGraph dir = do
  mm <- restoreContent dir
  runResourceT $ runConduit $
    citesFromFile dir .|
    C.foldl (\gr (CitesRow b a) ->
               let
                 edm = (,) <$> M.lookup a mm <*> M.lookup b mm
               in
                 case edm of
                   Nothing -> gr -- error $ show e
                   Just ((ib, bffs, bc), (ia, affs, ac)) ->
                     let
                       acr = CRow ia a affs ac
                       bcr = CRow ib b bffs bc
                     in
                       (acr `G.edge` bcr) `G.overlay` gr
                ) G.empty

-- | Stream out the edges of the citation graph, in which the nodes are decorated with the document metadata.
--
-- The full citation graph can be reconstructed by folding over this stream and `G.overlay`ing the graph edges as they arrive.
--
-- This way the graph can be partitioned in training , test and validation subsets at the usage site
sourceGraphEdges :: (MonadResource m, MonadThrow m) =>
                      FilePath -- ^ directory of data files
                   -> M.Map String (Int16, Seq Int16, c) -- ^ 'content' data
                   -> ConduitT i (Maybe (G.Graph (ContentRow Int16 c))) m ()
sourceGraphEdges dir mm =
    citesFromFile dir .|
    C.map (\(CitesRow b a) ->
             case (,) <$> M.lookup a mm <*> M.lookup b mm of
               Nothing -> Nothing
               Just ((ib, bffs, bc), (ia, affs, ac)) ->
                 let
                       acr = CRow ia a affs ac
                       bcr = CRow ib b bffs bc
                 in Just (acr `G.edge` bcr))


-- | Pick out the 'content' file in the archive, parse its contents and serialize to disk
--
-- | NB : the integer node identifiers are serialized as Int16, so the graph can only have up to 65535 nodes.
--
-- Contact customer service if you need more node IDs.
contentToFile :: (MonadThrow m, MonadResource m, Binary c) =>
                 FilePath
              -> Int -- ^ dictionary size
              -> Parser c -- ^ document class
              -> FileInfo
              -> ConduitT ByteString o m ()
contentToFile dir n pc fi = when ((takeExtension . unpack $ filePath fi) == ".content") $ do
  parseTSV .|
    C.map T.unwords .|
    void (C.mapAccumWhile ( \r i -> do
               case parse (contentRowP i n pc) "" r of
                 Left e -> error $ errorBundlePretty e
                 Right x -> Right (succ i, x) ) (0 :: Int16)
         ) .|
    CB.conduitEncode .|
    C.sinkFile (dir </> "content-z")

contentFromFile :: (MonadResource m, MonadThrow m, Binary c) => FilePath
                -> ConduitT i (ContentRow Int16 c) m ()
contentFromFile dir =
  C.sourceFile (dir </> "content-z") .|
  CB.conduitDecode


-- | Who cites whom
data CitesRow a = CitesRow {
  cirTo :: a -- ^ cited
  , cirFrom :: a -- ^ citing
  } deriving (Eq, Show, Generic, Binary)

citesRowP :: Parser (CitesRow String)
citesRowP = CitesRow <$> lexeme alphaNum <*> lexeme alphaNum


-- | Dataset row of the .content file
--
-- The .content file contains descriptions of the papers in the following format:
--
-- 		\<paper_id\> \<word_attributes\> \<class_label\>
--
-- The first entry in each line contains the unique string ID of the paper followed by binary values indicating whether each word in the vocabulary is present (indicated by 1) or absent (indicated by 0) in the paper. Finally, the last entry in the line contains the class label of the paper.
data ContentRow i c = CRow {
  crId :: i -- ^ integer identifier
  , crIdStr :: String -- ^ identifier string
  , crFeatures :: Seq Int16 -- ^ features, in sparse format (without the zeros)
  , crClass :: c -- ^ document class label
                   } deriving (Eq, Ord, Show, Generic, Binary)

bit :: Parser Bool
bit = (char '0' $> False) <|> (char '1' $> True)

sparse :: Foldable t => t Bool -> Seq Int16
sparse = fst . foldl (\(acc, i) b -> if b then (acc |> i, succ i) else (acc, succ i)) (mempty, 0)

contentRowP :: i -- ^ node identifier
            -> Int -- ^ vocabulary size
            -> Parser c -- ^ parser for document class
            -> Parser (ContentRow i c)
contentRowP i n dcp = do
  istr <- lexeme alphaNum
  feats <- sparse <$> count n (lexeme bit)
  c <- lexeme dcp
  pure $ CRow i istr feats c



citesToFile :: (MonadThrow m, MonadIO m, MonadResource m) =>
               FilePath
            -> FileInfo
            -> ConduitT ByteString c m ()
citesToFile dir fi = do
  let fpath = unpack $ filePath fi
  when (takeExtension fpath == ".cites") $
    parseTSV .|
    C.map T.unwords .|
    C.map ( \r -> case parse citesRowP "" r of
              Left e -> error $ errorBundlePretty e
              Right x -> x ) .|
    CB.conduitEncode .|
    C.sinkFile (dir </> "cites")



