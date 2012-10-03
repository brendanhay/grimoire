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

site :: Config m AppConfig -> Snap ()
site conf = route
    [ ("cookbooks/:name", json overview)
    , ("cookbooks/:name/versions/:version", json revision)
    , ("cookbooks/:name/versions/:version/archive", file archive)
    ]
  where
    file h = method GET $ h conf
    json h = h conf >>= method GET . writeLBS . encode

--
-- Private
--

archive :: Config m AppConfig -> Snap ()
archive conf = return ()

overview :: Config m AppConfig -> Snap Cookbook
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

revision :: Config m AppConfig -> Snap Cookbook
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

appConfig :: Config m AppConfig -> AppConfig
appConfig = fromJust . getOther

gitHub :: (Auth -> IO a) -> Config m AppConfig -> Snap a
gitHub f conf = liftIO $ f (_auth $ appConfig conf)

latest :: Config m AppConfig -> Name -> [Version] -> Maybe RevisionUri
latest _ _ []       = Nothing
latest conf n (v:_) = Just $ RevisionUri (uri conf n) v

uri :: Config m AppConfig -> Name -> Uri
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
