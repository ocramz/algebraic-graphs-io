{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.SV (
  tsvSink
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))
import Data.Void (Void)

-- algebraic-graphs
import qualified Algebra.Graph as G (Graph, edge, empty, overlay)
-- bytestring
import Data.ByteString (ByteString)
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield, await)
import qualified Data.Conduit.Combinators as C (print, sourceFile, sinkFile, map, mapM, foldM, mapWhile)
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
import Data.Conduit.Tar (Header(..), untarChunks, TarChunk, withEntries, headerFileType, FileType(..), headerFilePath)
-- text
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Algebra.Graph.IO.Internal.Megaparsec (Parser, ParseE)
import Algebra.Graph.IO.Internal.Conduit (unTarGz, fetch)




-- | Process chunks of a (uncompressed) TSV file and output the resulting graph
--
-- NB The TSV is assumed to have three columns, where the first two contain the node IDs of the edges
tsvSink :: (MonadThrow m) => ConduitT ByteString o m (G.Graph Int)
tsvSink = parseTSV .| C.map edgeP .| accGraph

parseTSV :: MonadThrow m => ConduitT ByteString (Row Text) m ()
parseTSV = intoCSV tsvSettings

edgeP :: [Text] -> Maybe (Edge Int)
edgeP t =
  case traverse (parse decimal "" :: Text -> Either ParseE Int) t of
    Left _ -> Nothing
    Right (a:b:c:_) -> Just $ Edge a b c
    Right _ -> Nothing

data Edge a = Edge a a a deriving (Eq, Show)

accGraph :: (Monad m) => ConduitT (Maybe (Edge a)) o m (G.Graph a)
accGraph = flip C.foldM G.empty $ \acc m -> 
  case m of
    Just (Edge a b _) -> pure $ (a `G.edge` b) `G.overlay` acc
    Nothing -> pure acc

-- | tab-separated values
tsvSettings :: CSVSettings
tsvSettings = CSVSettings '\t' Nothing



-- playground



-- test0 :: IO () -- (G.Graph Int)
-- test0 = do
--   rq <- parseRequest "https://graphchallenge.s3.amazonaws.com/synthetic/partitionchallenge/static/simulated_blockmodel_graph_50_nodes.tar.gz"
--   runResourceT $ runConduit $
--     fetch rq .|
--     unTarGz .|
--     parseTarEntry fname .|
--     C.print
--     where
--       fname :: FilePath
--       fname = "simulated_blockmodel_graph_50_nodes.tsv"


-- -- | Parse a single file from a .tar archive
-- parseTarEntry :: (MonadThrow m) =>
--                  FilePath -- ^ file in .tar archive
--               -> ConduitT TarChunk (G.Graph Int) m ()
-- parseTarEntry fname =
--   withEntries (\h -> when (headerFileType h == FTNormal &&
--                             headerFilePath h == fname) tsvC)


-- tsvC :: (MonadThrow m) => ConduitT ByteString (G.Graph Int) m ()
-- tsvC = do
--   g <- tsvSink
--   yield g
