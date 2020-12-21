{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.SV where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Void (Void)

-- bytestring
import Data.ByteString (ByteString)
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield)
import qualified Data.Conduit.Combinators as C (print, sinkFile)
-- conduit-extra
import Data.Conduit.Zlib (ungzip)
-- csv-conduit
import Data.CSV.Conduit (CSV(..), CSVSettings(..))
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- http-conduit
import Network.HTTP.Simple (httpSource, getResponseBody, Response, Request, parseRequest, setRequestMethod)
-- primitive
import Control.Monad.Primitive (PrimMonad(..))
-- tar-cnduit
import Data.Conduit.Tar (Header(..), untarChunks, withEntries, headerFileType, FileType(..), headerFilePath)

-- | tab-separated values
tsvSettings :: CSVSettings
tsvSettings = CSVSettings '\t' Nothing


fetch :: MonadResource m => Request -> ConduitT i ByteString m ()
fetch r = httpSource r getResponseBody

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

test0 :: IO ()
test0 = do
  rq <- parseRequest "https://graphchallenge.s3.amazonaws.com/synthetic/partitionchallenge/static/simulated_blockmodel_graph_50_nodes.tar.gz"
  runResourceT $ runConduit $
    fetch rq .|
    ungzip .|
    untarChunks .|
    withEntries ftypes .|
    C.print
    where
      ftypes h = when (headerFileType h == FTNormal) (yield $ headerFilePath h)
  
