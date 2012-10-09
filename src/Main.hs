-- |
-- Module      : Main
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main (
    -- * Main Entry Point
      main
    ) where

import Prelude                 hiding (lookup, catch)
import Control.Monad                  (liftM)
import Control.Monad.CatchIO          (catch, throw)
import Control.Monad.IO.Class         (liftIO)
import Data.Aeson                     (ToJSON, encode, object, (.=))
import Data.Maybe                     (fromJust)
import Data.String                    (IsString(..))
import Network.HTTP.Conduit           (HttpException(..))
import Network.HTTP.Types             (Status(..))
import Snap.Core
import Snap.Http.Server        hiding (Config)
import Snap.Util.FileServe            (serveFile)
import System.IO                      (BufferMode(..), stdout, hSetBuffering)
import Grimoire.Cache
import Grimoire.Config
import Grimoire.GitHub
import Grimoire.Types

import qualified Data.ByteString.Char8 as BS

type Key           = (Name, Version)
type RevisionCache = Cache Key Revision
type TarballCache  = Cache Key FilePath

main :: IO ()
main = do
    -- Ensure foreman flushes stdout
    hSetBuffering stdout NoBuffering

    -- Get some command line args
    httpConf <- parseConfig
    print httpConf

    -- Extract the important shit
    let appConf = fromJust $ getOther httpConf
    print appConf

    -- Setup type caches
    revs <- newCache
    tars <- newCache

    -- Start the serve with the site handlers
    httpServe httpConf $ site appConf revs tars

--
-- Handlers
--

site :: Config -> RevisionCache -> TarballCache -> Snap ()
site conf revs tars = method GET . route $ map runHandler
    [ ("cookbooks/:name", overview)
    , ("cookbooks/:name/versions/:version", revision revs)
    , ("cookbooks/:name/versions/:version/archive", archive tars)
    ]
  where
    runHandler (r, h) = (r, h conf `catch` handleException)

overview :: Config -> Snap ()
overview conf = do
    name <- requireParam "name"
    over <- liftIO $ getOverview name conf
    writeJson over

revision :: RevisionCache -> Config -> Snap ()
revision cache conf = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    rev  <- withCache cache (getRevision name ver conf) (name, ver)
    writeJson rev

archive :: TarballCache -> Config -> Snap ()
archive cache conf = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    file <- withCache cache (getTarball name ver conf) (name, ver)
    setDisposition file
    serveFile file

--
-- Helpers
--

handleException :: HttpException -> Snap ()
handleException e = do
    modifyResponse $ setResponseStatus code msg
    writeJson $ object [ "error" .= msg ]
    getResponse >>= finishWith
  where
    (code, msg) = parseException e

parseException :: HttpException -> (Int, BS.ByteString)
parseException e = case e of
    (StatusCodeException Status{..} _)  -> (statusCode, statusMessage)
    (InvalidUrlException _ msg)         -> (400, BS.pack msg)
    (HttpParserException msg)           -> (500, BS.pack msg)
    _                                   -> (500, "server error")

writeJson :: ToJSON j => j -> Snap ()
writeJson = writeLBS . encode

setDisposition :: FilePath -> Snap ()
setDisposition file = modifyResponse $ setHeader "Content-Disposition" val
  where
    name = snd . BS.spanEnd (not . (== '/')) $ BS.pack file
    val  = BS.concat ["attachment; filename=\"", name, "\""]

--
-- Params
--

class RequiredParam a where
    requireParam :: (MonadSnap m ) => BS.ByteString -> m a

instance RequiredParam String where
    requireParam = liftM BS.unpack . requireParam

instance RequiredParam Version where
    requireParam = liftM fromString . requireParam

instance RequiredParam BS.ByteString where
    requireParam name = do
        val <- getParam name
        case val of
            Just param ->
                return param
            _ ->
                throw $ InvalidUrlException "" ("Missing param " ++ BS.unpack name)
