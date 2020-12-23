{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Link-based datasets from https://linqs.soe.ucsc.edu/data
{-# options_ghc -Wno-unused-imports -Wno-unused-top-binds #-}
module Algebra.Graph.IO.Datasets.LINQS where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO(..))

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (unpack)
-- conduit
import Conduit (MonadUnliftIO(..), MonadResource, runResourceT)
import Data.Conduit (runConduit, ConduitT, (.|), yield, await)
import qualified Data.Conduit.Combinators as C (print, sourceFile, sinkFile, map, mapM, foldM, mapWhile)
-- exceptions
import Control.Monad.Catch (MonadThrow(..))
-- filepath
import System.FilePath ((</>), takeFileName, takeExtension)
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
import Data.Conduit.Tar (Header(..), untarChunks, TarChunk, withEntries, FileInfo, filePath, withFileInfo, headerFileType, FileType(..), headerFilePath)
-- text
import Data.Text (Text)

import Algebra.Graph.IO.Internal.Conduit (fetch, unTarGz)
import Algebra.Graph.IO.SV (parseTSV)

{-
CiteSeer: The CiteSeer dataset consists of 3312 scientific publications classified into one of six classes. The citation network consists of 4732 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 3703 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/citeseer.tgz

Cora: The Cora dataset consists of 2708 scientific publications classified into one of seven classes. The citation network consists of 5429 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 1433 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/cora.tgz

WebKB: The WebKB dataset consists of 877 scientific publications classified into one of five classes. The citation network consists of 1608 links. Each publication in the dataset is described by a 0/1-valued word vector indicating the absence/presence of the corresponding word from the dictionary. The dictionary consists of 1703 unique words. The README file in the dataset provides more details.
http://www.cs.umd.edu/~sen/lbc-proj/data/WebKB.tgz

-}
