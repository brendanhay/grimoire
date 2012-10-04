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

import Control.Monad.IO.Class (liftIO)
import Control.Monad          (liftM)
import Data.Aeson             (encode)
import Data.Maybe             (fromJust)
import Data.String            (IsString(..))
import Snap.Core
import Snap.Http.Server
import Snap.Util.FileServe    (serveFile)
import Grimoire.GitHub
import Grimoire.Types

import qualified Data.ByteString.Char8 as BS
import qualified Grimoire.ArchiveCache as AC

site :: Config m AppConfig -> Snap ()
site app = do
    cache <- liftIO $ AC.empty (_cacheDir conf) (_auth conf)
    method GET $ route
        [ ("cookbooks/:name", json overview)
        , ("cookbooks/:name/versions/:version", json revision)
        , ("cookbooks/:name/versions/:version/archive", archive conf cache)
        ]
  where
    json h = h conf >>= writeLBS . encode
    conf   = fromJust $ getOther app

--
-- Handlers
--

overview :: AppConfig -> Snap Cookbook
overview conf = do
    name <- requireParam "name"
    rep  <- applyAuth (getRepository name) conf
    ver  <- applyAuth (getVersions name) conf
    return $ toOverview (fromJust rep) ver conf

revision :: AppConfig -> Snap Cookbook
revision conf = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    rep  <- applyAuth (getRepository name) conf
    return $ toRevision (fromJust rep) ver conf

archive :: AppConfig -> AC.ArchiveCache -> Snap ()
archive conf cache = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    file <- liftIO $ AC.lookup (ArchiveUri (baseUri conf name) ver) cache
    serveFile file

--
-- Helpers
--

toOverview :: Repository -> [Version] -> AppConfig -> Cookbook
toOverview Repository{..} vers conf = Overview
    { name          = repoName
    , description   = repoDescription
    , latestVersion = latest conf repoName vers
    , versions      = map (RevisionUri $ baseUri conf repoName) vers
    , maintainer    = repoOwner
    , createdAt     = repoCreated
    , updatedAt     = repoUpdated
    }

toRevision :: Repository -> Version -> AppConfig -> Cookbook
toRevision Repository{..} ver conf = Revision
    { cookbook  = baseUri conf repoName
    , file      = ArchiveUri (baseUri conf repoName) ver
    , version   = ver
    , createdAt = repoCreated
    , updatedAt = repoUpdated
    }

applyAuth :: (Auth -> IO a) -> AppConfig -> Snap a
applyAuth f = liftIO . f . _auth

latest :: AppConfig -> Name -> [Version] -> Maybe RevisionUri
latest _ _ []            = Nothing
latest conf name (ver:_) = Just $ RevisionUri (baseUri conf name) ver

baseUri :: AppConfig -> Name -> Uri
baseUri AppConfig{..} = Uri _host _port

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
