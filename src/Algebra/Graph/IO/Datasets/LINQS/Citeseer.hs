{-# language DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports -Wno-unused-top-binds #-}
module Algebra.Graph.IO.Datasets.LINQS.Citeseer (citeseerGraph, stash, ContentRow(..), DocClass(..)) where

import Control.Applicative (Alternative(..))
import Control.Monad (when, foldM)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Generics (Generic(..))
import GHC.Int (Int16)
import Data.Functor (($>))

-- algebraic-graphs
import qualified Algebra.Graph as G (Graph, empty, overlay, edge)
-- binary
import Data.Binary (Binary(..), encode, decode, encodeFile, decodeFileOrFail)
-- binary-conduit
import qualified Data.Conduit.Serialization.Binary as CB (conduitDecode, conduitEncode, ParseError(..))
-- bytestring
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield, await)
import qualified Data.Conduit.Combinators as C (print, sourceFile, sinkFile, map, mapM, foldM, foldMap, foldl, foldMapM, mapWhile)
-- containers
import Data.Sequence (Seq, (|>))
import qualified Data.Map as M (Map, singleton, lookup)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- filepath
import System.FilePath ((</>), takeFileName, takeExtension)
-- http-conduit
import Network.HTTP.Simple (httpSource, getResponseBody, Response, Request, parseRequest, setRequestMethod)
-- megaparsec
import Text.Megaparsec (parse, parseTest, (<?>))
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Error (errorBundlePretty)
-- parser.combinators
import Control.Monad.Combinators (count)
-- primitive
import Control.Monad.Primitive (PrimMonad(..))
-- tar-conduit
import Data.Conduit.Tar (Header(..), untarChunks, TarChunk, withEntries, FileInfo, filePath, withFileInfo, headerFileType, FileType(..), headerFilePath)
-- text
import qualified Data.Text as T (Text, unwords)
import qualified Data.Text.IO as T (readFile)

import Algebra.Graph.IO.Internal.Conduit (fetch, unTarGz)
import Algebra.Graph.IO.Internal.Megaparsec (Parser, ParseE, symbol, lexeme, alphaNum)
import Algebra.Graph.IO.SV (parseTSV)

{-
CiteSeer: The CiteSeer dataset consists of 3312 scientific publications classified into one of six classes. The citation network consists of 4732 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 3703 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz
-}

-- | Download, parse, serialize and save the dataset to local storage.
--
-- Two binary files will be created under @.\/assets\/citeseer/@
stash :: IO ()
stash = do
  let path = "http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz"
  rq <- parseRequest path
  runResourceT $ runConduit $
    fetch rq .|
    unTarGz .|
    withFileInfo ( \fi -> do
     contentToFile fi
     citesToFile fi )

restoreContent :: IO (M.Map String (Seq Int16, DocClass))
restoreContent = runResourceT $ runConduit $
  contentFromFile .|
  C.foldMap ( \(CRow k fs c) -> M.singleton k (fs, c) )


-- | document classes of the Citeseer dataset
data DocClass = Agents | AI | DB | IR | ML | HCI deriving (Eq, Show, Generic, Binary)

docClassP :: Parser DocClass
docClassP =
  (symbol "Agents" $> Agents) <|>
  (symbol "AI" $> AI) <|>
  (symbol "DB" $> DB) <|>
  (symbol "IR" $> IR) <|>
  (symbol "ML" $> ML) <|>
  (symbol "HCI" $> HCI)

{-
The .content file contains descriptions of the papers in the following format:

		<paper_id> <word_attributes>+ <class_label>

The first entry in each line contains the unique string ID of the paper followed by binary values indicating whether each word in the vocabulary is present (indicated by 1) or absent (indicated by 0) in the paper (vocabulary : 3703 unique words). Finally, the last entry in the line contains the class label of the paper.
-}

-- | only process the .content file within the archive
content :: (MonadThrow io, MonadIO io) => ConduitT TarChunk o io ()
content = withFileInfo $ \fi ->
  when ((takeExtension . unpack $ filePath fi) == ".content") $
    parseTSV .|
    C.map T.unwords .|
    C.map (parse contentRowP "") .|
    C.print

contentToFile :: (MonadThrow m, MonadResource m) =>
                 FileInfo -> ConduitT ByteString c m ()
contentToFile fi = when ((takeExtension . unpack $ filePath fi) == ".content") $ do
  parseTSV .|
    C.map T.unwords .|
    C.map ( \r -> case parse contentRowP "" r of
              Left e -> error $ errorBundlePretty e
              Right x -> x ) .|
    CB.conduitEncode .|
    C.sinkFile "assets/citeseer/content-z"

contentFromFile :: (MonadResource m, MonadThrow m) => ConduitT i ContentRow m ()
contentFromFile =
  C.sourceFile "assets/citeseer/content-z" .|
  CB.conduitDecode

-- | Dataset row of the .content file
data ContentRow = CRow {
  crId :: String -- ^ identifier
  , crFeatures :: Seq Int16 -- ^ features, in sparse format (without the zeros)
  , crClass :: DocClass
                   } deriving (Eq, Show, Generic, Binary)

bit :: Parser Bool
bit = (char '0' $> False) <|> (char '1' $> True)

sparse :: Foldable t => t Bool -> Seq Int16
sparse = fst . foldl (\(acc, i) b -> if b then (acc |> i, succ i) else (acc, succ i)) (mempty, 0)

contentRowP :: Parser ContentRow
contentRowP = do
  i <- lexeme alphaNum
  let n = 3703
  foh <- count n (lexeme bit) -- one-hot encoded features
  let feats = sparse foh
  c <- lexeme docClassP
  pure $ CRow i feats c





{-
The .cites file contains the citation graph of the corpus. Each line describes a link in the following format:

		<ID of cited paper> <ID of citing paper>

Each line contains two paper IDs. The first entry is the ID of the paper being cited and the second ID stands for the paper which contains the citation. The direction of the link is from right to left. If a line is represented by "paper1 paper2" then the link is "paper2->paper1". 
-}
-- | only process the .cites file within the archive

citesToFile :: (MonadThrow m, MonadIO m, MonadResource m) =>
               FileInfo -> ConduitT ByteString c m ()
citesToFile fi = do
  let fpath = unpack $ filePath fi
  when (takeExtension fpath == ".cites") $
    parseTSV .|
    C.map T.unwords .|
    C.map ( \r -> case parse citesRowP "" r of
              Left e -> error $ errorBundlePretty e
              Right x -> x ) .|
    CB.conduitEncode .|
    C.sinkFile "assets/citeseer/cites"

citesFromFile :: (MonadResource m, MonadThrow m) => ConduitT i (CitesRow String) m ()
citesFromFile =
  C.sourceFile "assets/citeseer/cites" .|
  CB.conduitDecode

-- | Reconstruct the citation graph
--
-- NB : relies on the user having `stash`ed the dataset to local disk first.
citeseerGraph :: IO (G.Graph ContentRow)
citeseerGraph = do
  mm <- restoreContent
  runResourceT $ runConduit $
    citesFromFile .|
    C.foldl (\gr (CitesRow b a) ->
               let
                 edm = (,) <$> M.lookup a mm <*> M.lookup b mm
               in
                 case edm of
                   Nothing -> gr -- error $ show e
                   Just ((bffs, bc), (affs, ac)) ->
                     let
                       acr = CRow a affs ac
                       bcr = CRow b bffs bc
                     in
                       (acr `G.edge` bcr) `G.overlay` gr
                ) G.empty

data CitesRow a = CitesRow { cirTo :: a, cirFrom :: a } deriving (Eq, Show, Generic, Binary)

citesRowP :: Parser (CitesRow String)
citesRowP = CitesRow <$> lexeme alphaNum <*> lexeme alphaNum


-- test

-- -- | one row of the .content file
-- --
-- -- λ> content0
-- -- CRow {crId = "100157", crFeatures = fromList [36,46,65,215,261,565,1162,1508,1613,1641,1662,1797,1842,1988,2025,2399,2456,2521,2597,2618,2641,2902,3016,3050,3163,3268,3272,3287,3411,3447,3669], crClass = Agents}
-- content0 = do
--   t <- T.readFile "src/Algebra/Graph/IO/Datasets/LINQS/c0"
--   parseTest contentRowP t
