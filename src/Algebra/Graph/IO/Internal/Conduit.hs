{-# options_ghc -Wno-unused-imports #-}
-- | Miscellaneous conduit-related functionality
--
-- Networking, compression
module Algebra.Graph.IO.Internal.Conduit (fetchTarGz, unTarGz, fetch) where

import Control.Monad (when)

-- bytestring
import Data.ByteString (ByteString)
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield, await)
import qualified Data.Conduit.Combinators as C (print, sourceFile, sinkFile, map, mapM, foldM, mapWhile)
-- conduit-extra
import Data.Conduit.Zlib (ungzip)
-- filepath
import System.FilePath ((</>))
-- http-conduit
import Network.HTTP.Simple (httpSource, getResponseBody, Response, Request, parseRequest, setRequestMethod)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- primitive
import Control.Monad.Primitive (PrimMonad(..))
-- tar-conduit
import Data.Conduit.Tar (Header(..), untarChunks, TarChunk, withEntries, headerFileType, FileType(..), headerFilePath)


-- | Decompress a .tar.gz stream
unTarGz :: (PrimMonad m, MonadThrow m) => ConduitT ByteString TarChunk m ()
unTarGz = ungzip .|
          untarChunks

-- | Download a file
fetch :: MonadResource m => Request -> ConduitT i ByteString m ()
fetch r = httpSource r getResponseBody

-- | Download, decompress and save a .tar.gz archive
fetchTarGz :: String -- ^ URL with the .tar.gz
           -> FilePath -- ^ directory where to store archive contents
           -> IO ()
fetchTarGz path fp = do
  rq <- parseRequest path
  runResourceT $ runConduit $
    fetch rq .|
    unTarGz .|
    withEntries (\h -> when (headerFileType h == FTNormal) (C.sinkFile (fp </> headerFilePath h)))

untarEntries :: MonadThrow m =>
              (Header -> Bool)
           -> ConduitT ByteString o m () -- ^ process the content of each file that satisfies the predicate
           -> ConduitT TarChunk o m ()
untarEntries f p = withEntries (\h -> when (f h) p)

