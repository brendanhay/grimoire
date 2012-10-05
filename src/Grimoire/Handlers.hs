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

import Prelude         hiding (lookup)
import Control.Monad.IO.Class (liftIO)
import Control.Monad          (liftM)
import Data.Aeson             (encode)
import Data.Maybe             (fromJust)
import Data.String            (IsString(..))
import Snap.Core
import Snap.Util.FileServe    (serveFile)
import Grimoire.GitHub
import Grimoire.Types
import Grimoire.Cache

import qualified Data.ByteString.Char8     as BS
import qualified Grimoire.Cache.Repository as R
import qualified Grimoire.Cache.Archive    as A

site :: AppConfig -> R.Cache -> A.Cache -> Snap ()
site conf repos archives = method GET $ route
    [ ("cookbooks/:name", overview conf)
    , ("cookbooks/:name/versions/:version", revision conf repos)
    , ("cookbooks/:name/versions/:version/archive", archive conf archives)
    ]

--
-- Handlers
--

overview :: AppConfig -> Snap ()
overview conf = do
    name <- requireParam "name"
    rep  <- applyAuth (getRepository name) conf
    ver  <- applyAuth (getVersions name) conf
    json $ toOverview (fromJust rep) ver conf

revision :: AppConfig -> R.Cache -> Snap ()
revision conf cache = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    repo <- liftIO $ lookup name cache
    json $ toRevision conf ver repo

archive :: AppConfig -> A.Cache -> Snap ()
archive conf cache = do
    name <- requireParam "name"
    ver  <- requireParam "version"
    file <- liftIO $ lookup (ArchiveUri (baseUri conf name) ver) cache
    setDisposition file
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

toRevision :: AppConfig -> Version -> Repository -> Cookbook
toRevision conf ver Repository{..} = Revision
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

json :: Cookbook -> Snap ()
json = writeLBS . encode

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
