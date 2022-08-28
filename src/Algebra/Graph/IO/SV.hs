{-# language OverloadedStrings #-}
{-# options_ghc -Wno-unused-imports #-}
module Algebra.Graph.IO.SV (
  -- * Parse rows of a TSV file
  tsvSink,
  -- ** Labelled graphs
  tsvSinkL,
  -- * Utilities
  parseTSV,
  ) where

-- algebraic-graphs
import qualified Algebra.Graph as G (Graph, edge, empty, overlay)
import qualified Algebra.Graph.Labelled as GL (Graph, edge, empty, connect, overlay)
-- bytestring
import Data.ByteString (ByteString)
-- conduit
import Data.Conduit (ConduitT, (.|))
import qualified Data.Conduit.Combinators as C (map, foldM)
-- csv-conduit
import Data.CSV.Conduit (CSV(..), CSVSettings(..), Row)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- megaparsec
import Text.Megaparsec (parse, sepBy)
import Text.Megaparsec.Char.Lexer (decimal)
-- text
import Data.Text (Text)

import Algebra.Graph.IO.Internal.Megaparsec (Parser, ParseE)


-- | fetch chunks of a (uncompressed) TSV file and output the resulting graph
--
-- NB The TSV is assumed to have three columns, where the first two contain the node IDs of the edges
tsvSink :: (MonadThrow m) => ConduitT ByteString o m (G.Graph Int)
tsvSink = parseTSV .| C.map edgeP .| accGraph

-- | same as 'tsvSink', but uses the third TSV column as edge label
tsvSinkL :: (MonadThrow m) => ConduitT ByteString o m (GL.Graph [Int] Int)
tsvSinkL = parseTSV .| C.map edgeP .| accLGraph

parseTSV :: MonadThrow m => ConduitT ByteString (Row Text) m ()
parseTSV = intoCSV tsvSettings

edgeP :: Row Text -> Maybe (Edge [Int] Int)
edgeP = rowToEdge decimal decimal

rowToEdge :: Parser a -- ^ node IDs
          -> Parser e -- ^ edge labels
          -> Row Text -- ^ TSV row contents
          -> Maybe (Edge [e] a)
rowToEdge nodeP labelP t = case t of
  (ta:tb:te:_) -> case (parseNode ta, parseNode tb, parseEdge te) of
    (Right a, Right b, Right e) -> pure (Edge a b [e])
    _ -> Nothing
  _ -> Nothing
  where
    parseNode = parse nodeP ""
    parseEdge = parse labelP ""


-- | Labeled graph edges
data Edge e a = Edge a a e deriving (Eq, Show)

accGraph :: (Monad m) => ConduitT (Maybe (Edge e a)) o m (G.Graph a)
accGraph = flip C.foldM G.empty $ \acc m ->
  case m of
    Just (Edge a b _) -> pure $ (a `G.edge` b) `G.overlay` acc
    Nothing -> pure acc

accLGraph :: (Monad m) => ConduitT (Maybe (Edge [e] a)) o m (GL.Graph [e] a)
accLGraph = flip C.foldM GL.empty $ \acc m -> 
  case m of
    Just (Edge a b [e]) -> pure $ (GL.edge [e] a b) `GL.overlay` acc
    _ -> pure acc

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
