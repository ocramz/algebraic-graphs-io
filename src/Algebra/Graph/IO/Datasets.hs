{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.Datasets where

import Algebra.Graph (Graph)
import Algebra.Graph.IO.GML (GMLGraph, gmlGraph, gmlGraphP)
import Algebra.Graph.IO.Internal.Megaparsec (Parser, anyString)

import Text.Megaparsec (parse)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.Megaparsec.Char.Lexer (decimal)

import Data.Text.IO (readFile)

import Prelude hiding (readFile)

lesMiserables :: IO (Graph Int)
lesMiserables = do
  t <- readFile "assets/lesmiserables.gml"
  case parse (gmlGraphP decimal decimal) "" t of
    Right gg -> pure $ gmlGraph gg
    Left e -> error $ errorBundlePretty e

karateClub :: IO (Graph Int)
karateClub = do
  t <- readFile "assets/karate.gml"
  case parse (gmlGraphP decimal anyString) "" t of
    Right gg -> pure $ gmlGraph gg
    Left e -> error $ errorBundlePretty e
