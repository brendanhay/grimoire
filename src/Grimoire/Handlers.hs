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
import Grimoire.GitHub
import Grimoire.Types
import Grimoire.FileCache

import qualified Data.ByteString.Char8 as BS

type Config' = Config Snap AppConfig

site :: Config' -> Snap ()
site conf = method GET $ route
    [ ("cookbooks/:name", json overview)
    , ("cookbooks/:name/versions/:version", json revision)
    , ("cookbooks/:name/versions/:version/archive", file archive)
    ]
  where
    file h = h conf
    json h = h conf >>= writeLBS . encode

--
-- Private
--

overview :: Config' -> Snap Cookbook
overview conf = do
    name <- requireParam "name"
    rep  <- gitHub (repo name) conf
    ver  <- gitHub (vers name) conf
    return $ result (fromJust rep) ver
  where
    result Repository{..} v = Overview
        { name          = repoName
        , description   = repoDescription
        , latestVersion = latest conf repoName v
        , versions      = map (RevisionUri $ uri conf repoName) v
        , maintainer    = repoOwner
        , createdAt     = repoCreated
        , updatedAt     = repoUpdated
        }

revision :: Config' -> Snap Cookbook
revision conf = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    rep  <- gitHub (repo name) conf
    return $ result (fromJust rep) ver
  where
    result Repository{..} v = Revision
        { cookbook  = uri conf repoName
        , file      = ArchiveUri (uri conf repoName) v
        , version   = v
        , createdAt = repoCreated
        , updatedAt = repoUpdated
        }

archive :: Config' -> Snap ()
archive conf = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    serveArchive (ArchiveUri (uri conf name) ver) $ appConfig conf

appConfig :: Config' -> AppConfig
appConfig = fromJust . getOther

gitHub :: (Auth -> IO a) -> Config' -> Snap a
gitHub f conf = liftIO $ f (_auth $ appConfig conf)

latest :: Config' -> Name -> [Version] -> Maybe RevisionUri
latest _ _ []       = Nothing
latest conf n (v:_) = Just $ RevisionUri (uri conf n) v

uri :: Config' -> Name -> Uri
uri conf = Uri host port
  where
    f g  = fromJust $ g conf
    host = f getHostname
    port = f getPort

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
