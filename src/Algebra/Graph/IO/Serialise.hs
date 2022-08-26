{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-|
Module      : Algebra.Graph.IO.Serialise
Description : 'serialise' instances for algebraic-graphs types
Copyright   : (c) Marco Zocca, 2022
Maintainer  : ocramz
Stability   : experimental
Portability : POSIX

Orphan instances for compatibility between 'algebraic-graphs' and 'serialise'.

Import only if you know what you're doing.
-}
module Algebra.Graph.IO.Serialise () where

-- alga
import qualified Algebra.Graph as G (Graph(..), edges, foldg)
import qualified Algebra.Graph.Labelled as GL (Graph(..), edges, foldg)
-- serialise
import qualified Codec.Serialise as CS (Serialise(..), serialise, serialiseIncremental, deserialiseOrFail, DeserialiseFailure)
import qualified Codec.Serialise.Encoding as CS (encodeListLen, encodeListLenIndef, encodeWord)
import qualified Codec.Serialise.Decoding as CS (decodeListLen, decodeListLenIndef, decodeWord)


instance CS.Serialise a => CS.Serialise (G.Graph a) where
  encode = \case
    G.Empty -> CS.encodeListLen 1 <> CS.encodeWord 0
    G.Vertex x -> CS.encodeListLen 2 <> CS.encodeWord 1 <> CS.encode x
    G.Overlay a b -> CS.encodeListLen 3 <> CS.encodeWord 2 <> CS.encode a <> CS.encode b
    G.Connect a b -> CS.encodeListLen 3 <> CS.encodeWord 3 <> CS.encode a <> CS.encode b
  decode = do
    n <- CS.decodeListLen
    t <- CS.decodeWord -- constructor tag
    case (t, n) of
      (0, 1) -> pure $ G.Empty
      (1, 2) -> do
        !x <- CS.decode
        pure $ G.Vertex x
      (2, 3) -> do
        !x <- CS.decode
        !y <- CS.decode
        pure $ G.Overlay x y
      (3, 3) -> do
        !x <- CS.decode
        !y <- CS.decode
        pure $ G.Connect x y
      e -> fail $ unwords ["unknown tag", show e]

instance (CS.Serialise e, CS.Serialise a) => CS.Serialise (GL.Graph e a) where
  encode = \case
    GL.Empty -> CS.encodeListLen 1 <> CS.encodeWord 0
    GL.Vertex x -> CS.encodeListLen 2 <> CS.encodeWord 1 <> CS.encode x
    GL.Connect e a b -> CS.encodeListLen 4 <> CS.encodeWord 2 <> CS.encode e <> CS.encode a <> CS.encode b
  decode = do
    n <- CS.decodeListLen
    t <- CS.decodeWord -- constructor tag
    case (t, n) of
      (0, 1) -> pure $ GL.Empty
      (1, 2) -> do
        !x <- CS.decode
        pure $ GL.Vertex x
      (2, 4) -> do
        !e <- CS.decode
        !x <- CS.decode
        !y <- CS.decode
        pure $ GL.Connect e x y
      e -> fail $ unwords ["unknown tag", show e]
