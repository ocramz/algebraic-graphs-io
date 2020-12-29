{-# language DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports -Wno-unused-top-binds #-}
-- | Cora document classification dataset, from :
--
-- McCallum, A. and Nigam, K., "Automating the construction of internet portals with machine learning" Information Retrieval, 2000
--
-- Qing Lu, and Lise Getoor. "Link-based classification." ICML, 2003.
--
-- https://linqs.soe.ucsc.edu/data
module Algebra.Graph.IO.Datasets.LINQS.Cora (
    -- * 1. Download the dataset
  stash
  -- * 2. Reconstruct the citation graph
  , sourceCoraGraphEdges, loadCoraGraph
  -- * Types
  , CoraDoc(..)
                                            ) where

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

import Algebra.Graph.IO.Internal.Megaparsec (Parser, ParseE, symbol, lexeme, alphaNum)

import qualified Algebra.Graph.IO.Datasets.LINQS as DL (stash, sourceGraphEdges, loadGraph, restoreContent, CitesRow(..), ContentRow(..))

{-
The Cora dataset consists of Machine Learning papers. These papers are classified into one of the following seven classes:
		Case_Based
		Genetic_Algorithms
		Neural_Networks
		Probabilistic_Methods
		Reinforcement_Learning
		Rule_Learning
		Theory
-}

-- | document classes of the Cora dataset
data CoraDoc = CB | GA | NN | PM | RL | RuL | Th deriving (Eq, Show, Ord, Enum, Generic, Binary)

docClassP :: Parser CoraDoc
docClassP =
  (symbol "Case_Based" $> CB) <|>
  (symbol "Genetic_Algorithms" $> GA) <|>
  (symbol "Neural_Networks" $> NN) <|>
  (symbol "Probabilistic_Methods" $> PM) <|>
  (symbol "Reinforcement_Learning" $> RL) <|>
  (symbol "Rule_Learning" $> RuL) <|>
  (symbol "Theory" $> Th)

{-
The papers were selected in a way such that in the final corpus every paper cites or is cited by atleast one other paper. There are 2708 papers in the whole corpus. 

After stemming and removing stopwords we were left with a vocabulary of size 1433 unique words. All words with document frequency less than 10 were removed.
-}

stash :: FilePath -> IO ()
stash fp = DL.stash fp "http://www.cs.umd.edu/~sen/lbc-proj/data/cora.tgz" 1433 docClassP

-- | See `DL.sourceGraphEdges`
sourceCoraGraphEdges :: (MonadResource m, MonadThrow m) =>
                      FilePath -- ^ directory of data files
                   -> M.Map String (Seq Int16, CoraDoc) -- ^ 'content' data
                   -> ConduitT i (Maybe (G.Graph (DL.ContentRow CoraDoc))) m ()
sourceCoraGraphEdges = DL.sourceGraphEdges

-- | See `DL.loadGraph`
loadCoraGraph :: FilePath -- ^ directory where the data files were saved
                  -> IO (G.Graph (DL.ContentRow CoraDoc))
loadCoraGraph = DL.loadGraph



{-


THE DIRECTORY CONTAINS TWO FILES:

The .content file contains descriptions of the papers in the following format:

		<paper_id> <word_attributes> <class_label>

The first entry in each line contains the unique string ID of the paper followed by binary values indicating whether each word in the vocabulary is present (indicated by 1) or absent (indicated by 0) in the paper. Finally, the last entry in the line contains the class label of the paper.

The .cites file contains the citation graph of the corpus. Each line describes a link in the following format:

		<ID of cited paper> <ID of citing paper>

Each line contains two paper IDs. The first entry is the ID of the paper being cited and the second ID stands for the paper which contains the citation. The direction of the link is from right to left. If a line is represented by "paper1 paper2" then the link is "paper2->paper1". 
-}
