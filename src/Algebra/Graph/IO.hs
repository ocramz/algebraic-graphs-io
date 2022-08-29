{-# OPTIONS_GHC -Wno-unused-imports #-}
module Algebra.Graph.IO where

-- alga
import qualified Algebra.Graph as G (Graph(..), edges, foldg)
import qualified Algebra.Graph.Labelled as GL (Graph(..), edges, foldg)
import qualified Algebra.Graph.ToGraph as G (ToGraph, toAdjacencyMap)
import qualified Algebra.Graph.AdjacencyMap as GAM (AdjacencyMap, adjacencyMap, edgeList, vertexList)
-- containers
import qualified Data.Map as M (Map, fromList, insert)
import qualified Data.Set as S (Set, fromList, insert)


-- data GraphIO e a = GraphIO {
--   gioNodes :: [a]
--   , gioEdges :: [Edge e a]
--                            }
-- data Edge
