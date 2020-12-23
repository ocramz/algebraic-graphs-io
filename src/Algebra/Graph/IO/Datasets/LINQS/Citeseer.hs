{-# LANGUAGE OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports -Wno-unused-top-binds #-}
module Algebra.Graph.IO.Datasets.LINQS.Citeseer where

import Control.Applicative (Alternative(..))
import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import GHC.Int (Int16)
import Data.Functor (($>))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield, await)
import qualified Data.Conduit.Combinators as C (print, sourceFile, sinkFile, map, mapM, foldM, mapWhile)
-- containers
import Data.Sequence (Seq, (|>))
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- filepath
import System.FilePath ((</>), takeFileName, takeExtension)
-- http-conduit
import Network.HTTP.Simple (httpSource, getResponseBody, Response, Request, parseRequest, setRequestMethod)
-- megaparsec
import Text.Megaparsec (parse, parseTest)
import Text.Megaparsec.Char (char)
import Text.Megaparsec.Char.Lexer (decimal)
-- parser.combinators
import Control.Monad.Combinators (count)
-- primitive
import Control.Monad.Primitive (PrimMonad(..))
-- tar-conduit
import Data.Conduit.Tar (Header(..), untarChunks, TarChunk, withEntries, FileInfo, filePath, withFileInfo, headerFileType, FileType(..), headerFilePath)
-- text
import Data.Text (Text)
import qualified Data.Text.IO as T (readFile)

import Algebra.Graph.IO.Internal.Conduit (fetch, unTarGz)
import Algebra.Graph.IO.Internal.Megaparsec (Parser, ParseE, symbol, lexeme, alphaNum)
import Algebra.Graph.IO.SV (parseTSV)

{-
CiteSeer: The CiteSeer dataset consists of 3312 scientific publications classified into one of six classes. The citation network consists of 4732 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 3703 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz
-}

citeseer :: IO ()
citeseer = do
  let path = "http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz"
  rq <- parseRequest path
  runResourceT $ runConduit $
    fetch rq .|
    unTarGz .|
    cites



-- document classes of the Citeseer dataset
data DocClass = Agents | AI | DB | IR | ML | HCI deriving (Eq, Show)

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

-- | Dataset row of the .content file
data CRow a = CRow {
  crId :: a
  , crFeatures :: Seq Int16
  , crClass :: DocClass
                   } deriving (Eq, Show)

bit :: Parser Bool
bit = (char '0' $> False) <|> (char '1' $> True)

sparse :: Foldable t => t Bool -> Seq Int16
sparse = fst . foldl (\(acc, i) b -> if b then (acc |> i, succ i) else (acc, succ i)) (mempty, 0)

cRowP :: Parser (CRow String)
cRowP = do
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
cites :: (MonadThrow io, MonadIO io) => ConduitT TarChunk o io ()
cites = withFileInfo $ \fi ->
  when ((takeExtension . unpack $ filePath fi) == ".cites") $
  parseTSV .|
  C.print



-- test

-- | one row of the .content file
--
-- λ> content0
-- CRow {crId = "100157", crFeatures = fromList [36,46,65,215,261,565,1162,1508,1613,1641,1662,1797,1842,1988,2025,2399,2456,2521,2597,2618,2641,2902,3016,3050,3163,3268,3272,3287,3411,3447,3669], crClass = Agents}
content0 = do
  t <- T.readFile "src/Algebra/Graph/IO/Datasets/LINQS/c0"
  parseTest cRowP t
