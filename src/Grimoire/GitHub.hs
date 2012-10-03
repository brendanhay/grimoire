-- |
-- Module      : Grimoire.GitHub
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.GitHub (
    -- * Exported Types
      Auth(..)
    , Repository(..)
    , Tag(..)

    -- * Functions
    , repo
    , vers
    , tags
    ) where

import Data.String
import Control.Applicative         ((<$>), (<*>), empty)
import Data.Aeson                  (decode')
import Data.Aeson.Types
import Data.List                   (sort)
import Data.Maybe                  (fromJust)
import Data.Text                   (Text)
import Data.Vector                 (Vector, toList)
import Network.HTTP.Conduit hiding (queryString, path)
import Grimoire.Types

import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data Repository = Repository
   { repoName        :: BS.ByteString
   , repoDescription :: BS.ByteString
   , repoOwner       :: UserName
   , repoCreated     :: Time
   , repoUpdated     :: Time
   , repoUrl         :: BS.ByteString
   } deriving (Show)

instance FromJSON Repository where
    parseJSON (Object o) = Repository
        <$> o .: "name"
        <*> o .: "description"
        <*> ((o .: "owner") >>= (.: "login"))
        <*> o .: "created_at"
        <*> o .: "updated_at"
        <*> o .: "html_url"
    parseJSON _ = empty

data Tag = Tag
    { tagName    :: BS.ByteString
    , tagArchive :: BS.ByteString
    } deriving (Show, Eq, Ord)

instance FromJSON Tag where
    parseJSON (Object o) = Tag
        <$> o .: "name"
        <*> o .: "tarball_url"
    parseJSON _ = empty

--
-- API
--

repo :: BS.ByteString -> Auth -> IO (Maybe Repository)
repo name auth = do
    body <- request (BS.intercalate "/" ["repos", org auth, name]) auth
    return (decode' body :: Maybe Repository)

vers :: BS.ByteString -> Auth -> IO [Version]
vers name auth = do
    tags' <- tags name auth
    return . map (fromString . BS.unpack . tagName) $ sort tags'

tags :: BS.ByteString -> Auth -> IO [Tag]
tags name auth = do
    body <- request (BS.intercalate "/" ["repos", org auth, name, "tags"]) auth
    return $ case decode' body :: Maybe (Vector Tag) of
        Just v  -> toList v
        Nothing -> []

--
-- Private
--

request :: BS.ByteString -> Auth -> IO BL.ByteString
request path auth = withManager $ \manager -> do
    Response _ _ _ body <- httpLbs (uri path auth) manager
    return body

uri :: BS.ByteString -> Auth -> Request m
uri path auth = case parseUrl $ BS.unpack url of
    Just r  -> applyBasicAuth (user auth) (pass auth) r
    Nothing -> error "Invalid request"
  where
    url = BS.concat [base, path]

base :: BS.ByteString
base = "https://api.github.com/"

org :: Auth -> Org
org = fromJust . authOrg

user :: Auth -> UserName
user = fromJust . authUser

pass :: Auth -> Password
pass = fromJust . authPass
