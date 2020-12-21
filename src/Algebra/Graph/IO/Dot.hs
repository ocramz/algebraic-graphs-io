{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.Dot where

import Dot (DotGraph(..), Strictness(..), Directionality(..), Statement(..), Element(..), NodeStatement(..), EdgeStatement(..), Attribute(..), Id(..))


{-
main :: IO ()
main = do
  putStrLn $ "dumping example dotgraph to " ++ target
  encodeToFile target example
 
target :: FilePath
target = "example/example.dot"

example :: DotGraph
example = DotGraph Strict Directed (Just "foobar")
  [ StatementNode $ NodeStatement "a1"
    [ Attribute "color" "blue"
    , Attribute "shape" "box"
    ]
  , StatementNode $ NodeStatement "a2" []
  , StatementEdge $ EdgeStatement (ListTwo "a1" "a2" ["a3"])
      [ Attribute "color" "red"
      ]
  ]

-}
