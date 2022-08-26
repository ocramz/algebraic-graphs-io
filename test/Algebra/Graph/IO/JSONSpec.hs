module Algebra.Graph.IO.JSONSpec where

import Data.Either (isRight)
-- aeson
import qualified Data.Aeson as A (FromJSON(..), ToJSON(..), encode, eitherDecode)
-- alga
import qualified Algebra.Graph as G (Graph(..), edges, foldg)
import qualified Algebra.Graph.Labelled as GL (Graph(..), edges, foldg)
-- hspec
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import Algebra.Graph.IO.JSON

spec :: Spec
spec = describe "codec rountrip" $ do
  it "Graph Int" $ do
    g0' `shouldSatisfy` isRight
  it "Graph [Char] Int" $ do
    g1' `shouldSatisfy` isRight


g0 :: G.Graph Int
g0 = G.edges [(0, 1), (1, 2), (2, 3), (1, 4)]

g0' :: Either String (G.Graph Int)
g0' = A.eitherDecode $ A.encode g0


g1 :: GL.Graph [Char] Int
g1 = GL.edges [("x", 0, 1), ("y", 1, 2), ("z", 2, 3), ("w", 1, 4)]

g1' :: Either String (GL.Graph [Char] Int)
g1' = A.eitherDecode $ A.encode g1
