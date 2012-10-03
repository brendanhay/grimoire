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

import qualified Data.ByteString.Char8 as BS

site :: Config m AppConfig -> Snap ()
site conf = method GET . route $ map json routes
  where
    json (u, r) = (u, r conf >>= writeLBS . encode)

--
-- Private
--

routes :: [(BS.ByteString, Config m AppConfig -> Snap Cookbook)]
routes = [ ("cookbooks/:name", overview)
         , ("cookbooks/:name/versions/:version", revision)
         ]

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
        , versions      = map (uri conf repoName) v
        , maintainer    = repoOwner
        , createdAt     = repoCreated
        , updatedAt     = repoUpdated
        }

revision :: Config m AppConfig -> Snap Cookbook
revision conf = do
    name    <- requireParam "name"
    -- version <- requireParam "version"
    rep  <- gitHub (repo name) conf
    ver  <- gitHub (vers name) conf
    return $ result (fromJust rep) ver
  where
    result Repository{..} v = Revision
        { cookbook  = fromJust $ latest conf repoName v
        , file      = "" -- BS.intercalate "/" ["https://github.com", org, name, "tarball", v]
        , version   = head v
        , createdAt = repoCreated
        , updatedAt = repoUpdated
        }

gitHub :: (Auth -> IO a) -> Config m AppConfig -> Snap a
gitHub f conf = liftIO $ f (_auth . fromJust $ getOther conf)

latest :: Config m AppConfig -> Name -> [Version] -> Maybe Uri
latest _ _ []       = Nothing
latest conf n (x:_) = Just $ uri conf n x

uri :: Config m AppConfig -> Name -> Version -> Uri
uri conf n v = Uri (fromJust $ getHostname conf) (fromJust $ getPort conf) n v

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
