{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      : Grimoire.Handlers
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.Handlers (
    -- * Site Handler
      site
    ) where

import Prelude                hiding (lookup)
import Control.Monad.IO.Class        (liftIO)
import Control.Monad                 (liftM)
import Data.Aeson                    (ToJSON, encode)
import Data.String                   (IsString(..))
import Snap.Core
import Snap.Util.FileServe           (serveFile)
import Grimoire.Cache
import Grimoire.GitHub
import Grimoire.Types

import qualified Data.ByteString.Char8   as BS

type Key           = (Name, Version)
type RevisionCache = Cache Key Revision
type TarballCache  = Cache Key FilePath

site :: AppConfig -> RevisionCache  -> TarballCache -> Snap ()
site conf revs tars = method GET $ route
    [ ("cookbooks/:name", overview conf)
    , ("cookbooks/:name/versions/:version", revision conf revs)
    , ("cookbooks/:name/versions/:version/archive", archive conf tars)
    ]

--
-- Handlers
--

overview :: AppConfig -> Snap ()
overview conf = do
    name <- requireParam "name"
    over <- liftIO $ getOverview name conf
    writeJson over

revision :: AppConfig -> RevisionCache -> Snap ()
revision conf cache = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    rev  <- withCache cache (getRevision name ver conf) (name, ver)
    writeJson rev

archive :: AppConfig -> TarballCache -> Snap ()
archive conf cache = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    file <- withCache cache (getTarball name ver conf) (name, ver)
    setDisposition file
    serveFile file

--
-- Helpers
--

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
    requireParam :: BS.ByteString -> Snap a

instance RequiredParam String where
    requireParam = liftM BS.unpack . requireParam

instance RequiredParam Version where
    requireParam = liftM fromString . requireParam

instance RequiredParam BS.ByteString where
    requireParam name = do
        val <- getParam name
        case val of
            Just param -> return param
            _ -> do
                modifyResponse $ setResponseStatus 400 ("Missing param " `BS.append` name)
                writeBS "400 Missing param"
                getResponse >>= finishWith
