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

import Control.Monad  (liftM)
import Data.Aeson     (ToJSON, encode)
import Data.String    (IsString(..))
import Snap.Core
import Grimoire.Types

import qualified Data.ByteString.Char8 as BS

site :: Snap ()
site = method GET $ route
    [ ("cookbooks/:name", jsonResponse overview)
    , ("cookbooks/:name/versions/:version", jsonResponse specific)
    ]

--
-- Private
--

overview :: Snap Cookbook
overview = do
    name <- requireParam "name"
    return Overview
        { name          = name
        , latestVersion = Uri name "2.0.0"
        , versions      = [Uri name "1.0.0", Uri name "2.0.0"]
        , createdAt     = "2009-09-26T00:51:36Z"
        , updatedAt     = "2009-09-26T00:51:36Z"
        }

specific :: Snap Cookbook
specific = do
    name    <- requireParam "name"
    version <- requireParam "version"
    return Specific
        { cookbook  = Uri name version
        , file      = "http://s3.amazonaws.com/community-files.opscode.com/cookbook_versions/tarballs/2105/original/apache2.tgz"
        , version   = version
        , createdAt = "2009-09-26T00:51:36Z"
        , updatedAt = "2009-09-26T00:51:36Z"
        }

--
-- Response
--

jsonResponse :: (Show a, ToJSON a) => Snap a -> Snap ()
jsonResponse res = res >>= writeLBS . encode

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

