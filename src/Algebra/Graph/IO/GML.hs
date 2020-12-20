{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
-- | Mostly-complete implementation of the GML format
--
-- https://en.wikipedia.org/wiki/Graph_Modelling_Language
module Algebra.Graph.IO.GML (gmlGraph, gmlGraphP, GMLGraph(..), GMLNode(..), GMLEdge(..)) where

import Control.Applicative hiding (many, some)
import Data.Char (isAlpha, isSpace)
import Data.Functor (void)
import Data.Void (Void)

-- algebraic-graphs
import qualified Algebra.Graph as G (Graph, empty, vertex, edge, overlay)
-- megaparsec
import Text.Megaparsec (Parsec, parse, parseTest, satisfy, (<?>))
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
-- parser-combinators
import Control.Monad.Combinators (many, some, between, skipManyTill)
-- text
import Data.Text (Text)
import Data.Text.IO (readFile)

import Prelude hiding (readFile, takeWhile)

import Algebra.Graph.IO.Internal.Megaparsec (Parser, lexeme, symbol, anyString)

-- | Construct a 'G.Graph' using the edge data contained in a 'GMLGraph'
gmlGraph :: GMLGraph a -> G.Graph a
gmlGraph (GMLGraph _ _ es) =
  foldl (\gr (GMLEdge a b _) -> G.edge a b `G.overlay` gr) G.empty es

-- | Graph entities of the GML graph format
data GMLGraph a = GMLGraph {
  gmlCreator :: Maybe String
  , gmlNodes :: [GMLNode a]
  , gmlEdges :: [GMLEdge a]
  } deriving (Show)

-- | Parser for the GML graph format
gmlGraphP :: Parser a -- ^ parser for node id's
          -> Parser (GMLGraph a)
gmlGraphP p = do
  header <- optional creator -- header
  void $ symbol "graph"
  sqBkts $ do
    ns <- many $ gmlNode p
    es <- many $ gmlEdge p
    pure $ GMLGraph header ns es

creator :: Parser String
creator = do
  void $ symbol "Creator"
  quoted $ many $ satisfy (/= '\"')

gmlLabel :: Parser String
gmlLabel = symbol "label" *> lexeme (quoted p)
  where
    p = many $ satisfy (/= '\"')

-- | GML nodes
data GMLNode a = GMLNode a (Maybe String) deriving (Show)

gmlNode :: Parser a -> Parser (GMLNode a)
gmlNode p = do
  void $ symbol "node"
  sqBkts $ do
    n <- symbol "id" *> lexeme p
    l <- optional gmlLabel
    pure $ GMLNode n l

sqBkts :: Parser a -> Parser a
sqBkts = between (symbol "[") (symbol "]")
quoted :: Parser a -> Parser a
quoted = between (symbol "\"") (symbol "\"")

-- | GML edges
data GMLEdge a = GMLEdge a a (Maybe String) deriving (Show)

gmlEdge :: Parser a -> Parser (GMLEdge a)
gmlEdge p = do
  void $ symbol "edge"
  sqBkts $ do
    a <- symbol "source" *> lexeme p
    b <- symbol "target" *> lexeme p
    l <- optional gmlLabel
    pure $ GMLEdge a b l


