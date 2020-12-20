{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.GML where

import Control.Applicative hiding (many, some)
import Data.Char (isAlpha)
import Data.Functor (void)
import Data.Void (Void)

-- algebraic-graphs
import Algebra.Graph (Graph)
-- megaparsec
import Text.Megaparsec (Parsec, parseTest, satisfy)
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L
-- parser-combinators
import Control.Monad.Combinators (many, some, between)
-- text
import Data.Text (Text)
import Data.Text.IO (readFile)

import Prelude hiding (readFile)

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

-- space consumer
sc :: Parser ()
sc = L.space
     space1
     (L.skipLineComment "//")
     (L.skipBlockComment "/*" "*/")

anyString :: Parser String
anyString = many (satisfy isAlpha)

tstr :: IO ()
tstr = do
  t <- readFile "assets/basic.gml"
  parseTest gmlGraph t


data GMLNode a = GMLNode a (Maybe String) deriving (Show)

gmlNode :: Parser (GMLNode String)
gmlNode = do
  void $ symbol "node"
  sqBkts $ do
    n <- symbol "id" *> lexeme anyString
    l <- optional $ symbol "label" *> lexeme anyString
    pure $ GMLNode n l

sqBkts :: Parser a -> Parser a
sqBkts = between (symbol "[") (symbol "]")

data GMLEdge a = GMLEdge a a (Maybe String) deriving (Show)

gmlEdge :: Parser (GMLEdge String)
gmlEdge = do
  void $ symbol "edge"
  sqBkts $ do
    a <- symbol "source" *> lexeme anyString
    b <- symbol "target" *> lexeme anyString
    l <- optional $ symbol "label" *> lexeme anyString
    pure $ GMLEdge a b l

data GMLGraph a = GMLGraph [GMLNode a] [GMLEdge a] deriving (Show)

gmlGraph :: Parser (GMLGraph String)
gmlGraph = do
  void $ symbol "graph"
  sqBkts $ do
    ns <- many gmlNode
    es <- many gmlEdge
    pure $ GMLGraph ns es

{-
graph
[
  node
  [
   id A
   label "Node A"
  ]
  node
  [
   id B
   label "Node B"
  ]
  node
  [
   id C
   label "Node C"
  ]
   edge
  [
   source B
   target A
   label "Edge B to A"
  ]
  edge
  [
   source C
   target A
   label "Edge C to A"
  ]
]
-}
