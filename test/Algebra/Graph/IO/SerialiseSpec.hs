module Algebra.Graph.IO.SerialiseSpec (spec) where

import Data.Either (isRight)

-- alga
import qualified Algebra.Graph as G (Graph(..), edges, foldg)
import qualified Algebra.Graph.Labelled as GL (Graph(..), edges, foldg)
-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
-- serialise
import qualified Codec.Serialise as CS (Serialise(..), serialise, serialiseIncremental, deserialiseOrFail, DeserialiseFailure)
import qualified Codec.Serialise.Encoding as CS (encodeListLen, encodeWord)
import qualified Codec.Serialise.Decoding as CS (decodeListLen, decodeWord)

import Algebra.Graph.IO.Serialise

spec :: Spec
spec = describe "codec rountrip" $ do
  it "Graph Int" $ do
    g0' `shouldSatisfy` isRight
  it "Graph [Char] Int" $ do
    g1' `shouldSatisfy` isRight

g0 :: G.Graph Int
g0 = G.edges [(0, 1), (1, 2), (2, 3), (1, 4)]

g0' :: Either CS.DeserialiseFailure (G.Graph Int)
g0' = CS.deserialiseOrFail $ CS.serialise g0



g1 :: GL.Graph [Char] Int
g1 = GL.edges [("x", 0, 1), ("y", 1, 2), ("z", 2, 3), ("w", 1, 4)]

g1' :: Either CS.DeserialiseFailure (GL.Graph [Char] Int)
g1' = CS.deserialiseOrFail $ CS.serialise g1
