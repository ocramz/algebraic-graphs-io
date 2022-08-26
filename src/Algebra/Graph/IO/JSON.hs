{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
{-|
Module      : Algebra.Graph.IO.JSON
Description : 'aeson' instances for algebraic-graphs types
Copyright   : (c) Marco Zocca, 2022
Maintainer  : ocramz
Stability   : experimental
Portability : POSIX

Orphan instances for compatibility between 'algebraic-graphs' and 'aeson'.

Import only if you know what you're doing.
-}
module Algebra.Graph.IO.JSON () where

import Control.Applicative (Alternative(..))
import GHC.Generics (Generic)

-- aeson
import qualified Data.Aeson as A (FromJSON(..), ToJSON(..), encode, eitherDecode, Value, withArray, withText, withObject, (.:), Object)
import qualified Data.Aeson.Types as A (Parser)
import qualified Data.Aeson.Encoding as A (value, fromEncoding)
-- alga
import qualified Algebra.Graph as G (Graph(..), edges, foldg)
import qualified Algebra.Graph.Labelled as GL (Graph(..), edges, foldg)

-- unlabeled edges

instance A.ToJSON a => A.ToJSON (G.Graph a) where
  toJSON = graphToValue


graphToValue :: (A.ToJSON t) =>
                G.Graph t -> A.Value
graphToValue = go
  where
    go G.Empty = A.toJSON Empty
    go (G.Vertex x) = A.toJSON (Vertex x)
    go (G.Overlay x y) = A.toJSON (Overlay (go x) (go y))
    go (G.Connect x y) = A.toJSON (Connect (go x) (go y))




instance A.FromJSON a => A.FromJSON (G.Graph a) where
  parseJSON x = A.withObject "Graph" gObj x <|>
                parseE x

gObj :: A.FromJSON a => A.Object -> A.Parser (G.Graph a)
gObj o = parseC o <|>
         parseO o <|>
         parseV o

parseE :: A.Value -> A.Parser (G.Graph a)
parseE = A.withArray "empty" $ \t -> if null t then pure G.Empty else fail "cannot parse Empty"

parseV :: A.FromJSON a => A.Object -> A.Parser (G.Graph a)
parseV o = G.Vertex <$> o A..: "v"

parseO :: A.FromJSON a => A.Object -> A.Parser (G.Graph a)
parseO o = do
  a <- o A..: "o1"
  b <- o A..: "o2"
  pure $ G.Overlay a b

parseC :: A.FromJSON a => A.Object -> A.Parser (G.Graph a)
parseC o = do
  a <- o A..: "c1"
  b <- o A..: "c2"
  pure $ G.Connect a b




instance (A.ToJSON a, A.ToJSON e) => A.ToJSON (GL.Graph e a) where
  toJSON = graphLToValue

graphLToValue :: (A.ToJSON a, A.ToJSON e) => GL.Graph e a -> A.Value
graphLToValue = go
  where
    go GL.Empty = A.toJSON Empty
    go (GL.Vertex x) = A.toJSON (Vertex x)
    go (GL.Connect e x y) = A.toJSON (LEdge e (go x) (go y))


instance (A.FromJSON e, A.FromJSON a) => A.FromJSON (GL.Graph e a) where
  parseJSON x = A.withObject "Graph (labeled)" gLObj x <|>
                parseEL x

gLObj :: (A.FromJSON e, A.FromJSON a) => A.Object -> A.Parser (GL.Graph e a)
gLObj o = parseCL o <|>
          parseVL o

parseEL :: A.Value -> A.Parser (GL.Graph e a)
parseEL = A.withArray "empty" $ \t -> if null t then pure GL.Empty else fail "cannot parse Empty"

parseVL :: A.FromJSON a => A.Object -> A.Parser (GL.Graph e a)
parseVL o = GL.Vertex <$> o A..: "v"

parseCL :: (A.FromJSON e, A.FromJSON a) =>
           A.Object -> A.Parser (GL.Graph e a)
parseCL o = do
  e <- o A..: "l"
  a <- o A..: "e1"
  b <- o A..: "e2"
  pure $ GL.Connect e a b


-- | Helper types

-- empty
data Empty = Empty deriving (Eq, Show, Generic)
instance A.ToJSON Empty
-- vertex
newtype Vertex a = Vertex { v :: a } deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Vertex a)

-- overlay
data Overlay a = Overlay { o1 :: a, o2 :: a} deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Overlay a)

-- connect
data Connect a = Connect { c1 :: a, c2 :: a} deriving (Eq, Show, Generic)
instance A.ToJSON a => A.ToJSON (Connect a)




data LEdge e a = LEdge { l :: e, e1 :: a, e2 :: a } deriving (Eq, Show, Generic)
instance (A.ToJSON e, A.ToJSON a) => A.ToJSON (LEdge e a)

