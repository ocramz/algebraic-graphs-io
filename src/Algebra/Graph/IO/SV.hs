{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.SV where

import Data.Conduit (runConduit, ConduitT)
import Data.CSV.Conduit (CSV(..), CSVSettings(..))

-- | tab-separated values
tsvSettings :: CSVSettings
tsvSettings = CSVSettings '\t' Nothing


{-
myProcessor :: Conduit (MapRow Text) m (MapRow Text)
myProcessor = undefined

test = runResourceT $ runConduit $
  sourceFile "test/BigFile.csv" .|
  intoCSV defCSVSettings .|
  myProcessor .|
  (writeHeaders defCSVSettings >> fromCSV defCSVSettings) .|
  sinkFile "test/BigFileOut.csv"
-}
