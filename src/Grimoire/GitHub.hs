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
    -- * Functions
      getOverview
    , getRevision
    , getTarball
    ) where

import Control.Monad (unless)
import Control.Monad.IO.Class      (liftIO)
import Data.String
import Control.Applicative         ((<$>), (<*>), empty)
import Data.Aeson                  (decode')
import Data.Aeson.Types
import Data.List                   (sort)
import Data.Maybe                  (fromJust)
import Data.Vector                 (Vector, toList)
import Network.HTTP.Conduit hiding (queryString, path)
import Grimoire.Types
import Data.Conduit.Binary          (sinkFile)
import System.Directory             (doesFileExist, createDirectoryIfMissing)

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

getOverview :: Name -> AppConfig -> IO Overview
getOverview name AppConfig{..} = do
    repo <- getRepository name _auth
    vers <- getVersions name _auth
    return $ toOverview repo vers _baseUri

getRevision :: Name -> Version -> AppConfig -> IO Revision
getRevision name ver AppConfig{..} = do
    repo <- getRepository name _auth
    return $ toRevision repo ver _baseUri

getTarball :: Name -> Version -> AppConfig -> IO FilePath
getTarball name ver AppConfig{..} = withManager $ \m -> do
    p <- liftIO $ doesFileExist file
    unless p $ do
        liftIO $ createDirectoryIfMissing True dir
        res <- http (wrapAuth url _auth) m
        responseBody res C.$$+- sinkFile file
    return file
  where
    (file, dir) = archivePaths name ver _cacheDir
    url         = tarball name ver _auth

--
-- Private
--

api :: BS.ByteString -> BS.ByteString
api path = BS.concat ["https://api.github.com/", path]

tarball :: Name -> Version -> Auth -> BS.ByteString
tarball name ver a = BS.intercalate "/"
    [ "https://github.com"
    , orgStr a
    , name
    , "tarball"
    , versionStr ver
    ]

getRepository :: Name -> Auth -> IO Repository
getRepository name a = do
    body <- request (BS.intercalate "/" ["repos", orgStr a, name]) a
    return $ fromJust (decode' body :: Maybe Repository)

getVersions :: Name -> Auth -> IO [Version]
getVersions name a = do
    tags' <- getTags name a
    return . map (fromString . BS.unpack . tagName) $ sort tags'

getTags :: Name -> Auth -> IO [Tag]
getTags n a = do
    body <- request (BS.intercalate "/" ["repos", orgStr a, n, "tags"]) a
    return $ case decode' body :: Maybe (Vector Tag) of
        Just v  -> toList v
        Nothing -> []

request :: BS.ByteString -> Auth -> IO BL.ByteString
request path a = withManager $ \m -> do
    Response _ _ _ body <- httpLbs (wrapAuth (api path) a) m
    return body

wrapAuth :: BS.ByteString -> Auth -> Request m
wrapAuth path a = case parseUrl $ BS.unpack path of
    Just r  -> applyBasicAuth (userStr a) (passStr a) r
    Nothing -> error "Invalid request"

toOverview :: Repository -> [Version] -> BaseUri -> Overview
toOverview Repository{..} vers base = Overview
    { ovName        = repoName
    , ovDescription = repoDescription
    , ovLatest      = latest vers
    , ovVersions    = map (RevisionUri $ base repoName) vers
    , ovMaintainer  = repoOwner
    , ovCreated     = repoCreated
    , ovUpdated     = repoUpdated
    }
  where
    latest (v:_) = Just $ RevisionUri (base repoName) v
    latest []    = Nothing

toRevision :: Repository -> Version -> BaseUri -> Revision
toRevision Repository{..} ver base = Revision
    { revCookbook = OverviewUri $ base repoName
    , revFile     = ArchiveUri (base repoName) ver
    , revVersion  = ver
    , revCreated  = repoCreated
    , revUpdated  = repoUpdated
    }

archivePaths :: Name -> Version -> BS.ByteString -> (FilePath, FilePath)
archivePaths name ver dir = (BS.unpack pref, BS.unpack file)
  where
    join = BS.intercalate "/"
    pref = join [dir, name]
    file = join [pref, BS.concat [name, "-", encodeUri ver, ".tar.gz"]]
