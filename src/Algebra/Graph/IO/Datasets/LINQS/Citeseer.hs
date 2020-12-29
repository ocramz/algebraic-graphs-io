{-# language DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports -Wno-unused-top-binds #-}
-- | Citeseer document classification dataset, from :
--
-- Qing Lu, and Lise Getoor. "Link-based classification." ICML, 2003.
--
-- https://linqs.soe.ucsc.edu/data
module Algebra.Graph.IO.Datasets.LINQS.Citeseer (
  -- * 1. Download the dataset
  stash
  -- * 2. Reconstruct the citation graph (`DL.sourceGraphEdges` or `DL.loadGraph`)
    -- * Types
    ,DocClass(..)) where

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
-- directory
import System.Directory (createDirectoryIfMissing)
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
import qualified Algebra.Graph.IO.Datasets.LINQS as DL (stash, restoreContent, CitesRow(..), ContentRow(..))
{-
CiteSeer: The CiteSeer dataset consists of 3312 scientific publications classified into one of six classes. The citation network consists of 4732 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 3703 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz
-}

stash :: FilePath -- ^ directory where the data files will be saved
      -> IO ()
stash fp = DL.stash fp "http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz" 3703 docClassP


-- | document classes of the Citeseer dataset
data DocClass = Agents | AI | DB | IR | ML | HCI deriving (Eq, Ord, Enum, Show, Generic, Binary)

docClassP :: Parser DocClass
docClassP =
  (symbol "Agents" $> Agents) <|>
  (symbol "AI" $> AI) <|>
  (symbol "DB" $> DB) <|>
  (symbol "IR" $> IR) <|>
  (symbol "ML" $> ML) <|>
  (symbol "HCI" $> HCI)


{-
The .cites file contains the citation graph of the corpus. Each line describes a link in the following format:

		<ID of cited paper> <ID of citing paper>

Each line contains two paper IDs. The first entry is the ID of the paper being cited and the second ID stands for the paper which contains the citation. The direction of the link is from right to left. If a line is represented by "paper1 paper2" then the link is "paper2->paper1". 
-}
-- | only process the .cites file within the archive






-- test

-- -- | one row of the .content file
-- --
-- -- λ> content0
-- -- CRow {crId = "100157", crFeatures = fromList [36,46,65,215,261,565,1162,1508,1613,1641,1662,1797,1842,1988,2025,2399,2456,2521,2597,2618,2641,2902,3016,3050,3163,3268,3272,3287,3411,3447,3669], crClass = Agents}
-- content0 = do
--   t <- T.readFile "src/Algebra/Graph/IO/Datasets/LINQS/c0"
--   parseTest contentRowP t
