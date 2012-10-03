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
    , tarball
    ) where

import Data.String
import Control.Applicative         ((<$>), (<*>), empty)
import Data.Aeson                  (decode')
import Data.Aeson.Types
import Data.List                   (sort)
import Data.Vector                 (Vector, toList)
import Network.HTTP.Conduit hiding (queryString, path)
import Grimoire.Types

import qualified Data.Conduit as C
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BL

data Repository = Repository
   { repoName        :: BS.ByteString
   , repoDescription :: BS.ByteString
   , repoOwner       :: User
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

repo :: Name -> Auth -> IO (Maybe Repository)
repo n a = do
    body <- request (BS.intercalate "/" ["repos", orgStr a, n]) a
    return (decode' body :: Maybe Repository)

vers :: Name -> Auth -> IO [Version]
vers n a = do
    tags' <- tags n a
    return . map (fromString . BS.unpack . tagName) $ sort tags'

tags :: Name -> Auth -> IO [Tag]
tags n a = do
    body <- request (BS.intercalate "/" ["repos", orgStr a, n, "tags"]) a
    return $ case decode' body :: Maybe (Vector Tag) of
        Just v  -> toList v
        Nothing -> []

tarball :: Name
        -> Version
        -> Auth
        -> C.Sink BS.ByteString (C.ResourceT IO) ()
        -> IO ()
tarball n v a sink = (withManager $ \m -> do
    res <- http (uri u a) m
    responseBody res C.$$+- sink)
  where
    u = BS.intercalate "/" [ "https://github.com"
                           , orgStr a
                           , n
                           , "tarball"
                           , versionStr v
                           ]

--
-- Private
--

request :: BS.ByteString -> Auth -> IO BL.ByteString
request path a = withManager $ \m -> do
    Response _ _ _ body <- httpLbs (uri path a) m
    return body

uri :: BS.ByteString -> Auth -> Request m
uri path a = case parseUrl $ BS.unpack url of
    Just r  -> applyBasicAuth (userStr a) (passStr a) r
    Nothing -> error "Invalid request"
  where
    url = BS.concat [base, path]

base :: BS.ByteString
base = "https://api.github.com/"
