{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.SV where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Void (Void)

-- algebraic-graphs
import qualified Algebra.Graph as G (Graph, edge, empty)
-- bytestring
import Data.ByteString (ByteString)
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield)
import qualified Data.Conduit.Combinators as C (print, sinkFile, map)
-- conduit-extra
import Data.Conduit.Zlib (ungzip)
-- csv-conduit
import Data.CSV.Conduit (CSV(..), CSVSettings(..), Row)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- http-conduit
import Network.HTTP.Simple (httpSource, getResponseBody, Response, Request, parseRequest, setRequestMethod)
-- megaparsec
import Text.Megaparsec (parse)
import Text.Megaparsec.Char.Lexer (decimal)
-- parser.combinators
import Control.Monad.Combinators (count)
-- primitive
import Control.Monad.Primitive (PrimMonad(..))
-- tar-conduit
import Data.Conduit.Tar (Header(..), untarChunks, withEntries, headerFileType, FileType(..), headerFilePath)
-- text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Algebra.Graph.IO.Internal.Megaparsec (Parser, ParseE)

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
    withEntries p .|
    C.map edgeP .|
    C.print
    where
      p h = when (headerFileType h == FTNormal) $ do -- (yield $ headerFilePath h)
        parseTSV


parseTSV :: MonadThrow m => ConduitT ByteString (Row Text) m ()
parseTSV = intoCSV tsvSettings


edgeP :: [Text] -> Maybe (Edge Int)
edgeP t =
  case traverse (parse decimal "" :: Text -> Either ParseE Int) t of
    Left _ -> Nothing
    Right (a:b:c:_) -> Just $ Edge a b c
    Right _ -> Nothing

data Edge a = Edge a a a deriving (Eq, Show)
