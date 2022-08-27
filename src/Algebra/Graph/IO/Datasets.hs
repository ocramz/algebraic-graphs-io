{-# options_ghc -Wno-unused-imports -Wno-type-defaults #-}
module Algebra.Graph.IO.Datasets where

import Algebra.Graph (Graph)
import qualified Algebra.Graph.ToGraph as G  (ToGraph(..))
import Algebra.Graph.IO.GML (GMLGraph, gmlGraphP)
import qualified Algebra.Graph.IO.SV as SV (tsvSink)
import Algebra.Graph.IO.Internal.Megaparsec (Parser, anyString)

-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield, await)
import qualified Data.Conduit.Combinators as C (print, sourceFile, sinkFile, map, mapM, foldM, mapWhile)
-- megaparsec
import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Text.IO (readFile)

import Prelude hiding (readFile)

-- | "Les Miserables" dataset
--
-- from https://github.com/gephi/gephi/wiki/Datasets
lesMiserables :: IO (Graph Int)
lesMiserables = do
  t <- readFile "assets/lesmiserables.gml"
  case parse (gmlGraphP decimal decimal) "" t of
    Right gg -> pure $ G.toGraph gg
    Left e -> error $ errorBundlePretty e

-- | "Karate club" dataset
--
-- from https://github.com/gephi/gephi/wiki/Datasets
karateClub :: IO (Graph Int)
karateClub = do
  t <- readFile "assets/karate.gml"
  case parse (gmlGraphP decimal anyString) "" t of
    Right gg -> pure $ G.toGraph gg
    Left e -> error $ errorBundlePretty e

-- | Small test dataset
--
-- from https://graphchallenge.mit.edu/data-sets
blockModel50 :: IO (Graph Int)
blockModel50 = runResourceT $ runConduit $
  C.sourceFile "assets/simulated_blockmodel_graph_50_nodes.tsv" .|
  SV.tsvSink
