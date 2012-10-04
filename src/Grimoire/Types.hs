{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Grimoire.Types
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.Types (
    -- * Exported Types
      Name
    , File
    , Org(..)
    , User(..)
    , Password(..)
    , Auth(..)
    , AppConfig(..)
    , Uri(..)
    , OverviewUri
    , RevisionUri(..)
    , ArchiveUri(..)
    , Time(..)
    , Cookbook(..)

    -- * Restricted Constructors
    , Version
    , versionStr

    -- * Type Classes
    , SafeUri(..)

    -- * Lenses
    , auth
    , authOrg
    , authUser
    , authPass

    -- * Accessors
    , orgStr
    , userStr
    , passStr
    ) where

import Control.Monad (liftM)
import Data.Aeson    (ToJSON(..), FromJSON(..), object, (.=))
import Data.Function (on)
import Control.Lens  hiding ((.=))
import Data.Monoid
import Data.String   (IsString(..))
import Data.UnixTime (UnixTime, Format, parseUnixTimeGMT, formatUnixTimeGMT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

type Name = BS.ByteString
type File = BL.ByteString

data Org = Org BS.ByteString
    deriving (Show, Eq)

instance Monoid Org where
    mempty  = "organisation"
    mappend = rappend

instance IsString Org where
    fromString = Org . fromString

data User = User Name
    deriving (Show, Eq)

instance Monoid User where
    mempty  = "user"
    mappend = rappend

instance IsString User where
    fromString = User . fromString

instance ToJSON User where
    toJSON (User bs) = toJSON bs

instance FromJSON User where
    parseJSON j = liftM User (parseJSON j)

data Password = Password BS.ByteString
    deriving (Show, Eq)

instance Monoid Password where
    mempty  = "password"
    mappend = rappend

instance IsString Password where
    fromString = Password . fromString

rappend :: (Monoid m, Eq m) => m -> m -> m
rappend a b | a == mempty = b
            | b == mempty = a
            | otherwise   = b

data Auth = Auth
    { _authOrg  :: Org
    , _authUser :: User
    , _authPass :: Password
    } deriving (Eq, Show)

$(makeLenses ''Auth)

orgStr, userStr, passStr :: Auth -> BS.ByteString
orgStr  Auth{..} = bs where Org bs      = _authOrg
userStr Auth{..} = bs where User bs     = _authUser
passStr Auth{..} = bs where Password bs = _authPass

instance Monoid Auth where
    mempty      = Auth mempty mempty mempty
    mappend a b = Auth
        { _authOrg  = ov _authOrg
        , _authUser = ov _authUser
        , _authPass = ov _authPass
        }
      where
        ov f = (mappend `on` f) a b

data AppConfig = AppConfig
    { _auth     :: Auth
    , _cacheDir :: BS.ByteString
    , _host     :: BS.ByteString
    , _port     :: Int
    } deriving (Show)

$(makeLenses ''AppConfig)

instance Monoid AppConfig where
    mempty      = AppConfig mempty ".cache" mempty 0
    mappend a b = AppConfig
        { _auth     = mappend (_auth a) (_auth b)
        , _cacheDir = rappend (_cacheDir a) (_cacheDir b)
        , _host     = _host b
        , _port     = _port b
        }

data Version = Version BS.ByteString
    deriving (Ord, Eq, Show)

versionStr :: Version -> BS.ByteString
versionStr (Version bs) = BS.map fn bs
  where
    fn '_' = '.'
    fn c   = c

instance IsString Version where
    fromString = Version . BS.pack

instance ToJSON Version where
    toJSON = toJSON . versionStr

class SafeUri a where
    encodeUri :: a -> BS.ByteString

data Uri = Uri
    { _uriHost     :: BS.ByteString
    , _uriPort     :: Int
    , _uriCookbook :: Name
    } deriving (Ord, Eq, Show)

$(makeLenses ''Uri)

type OverviewUri = Uri

data RevisionUri = RevisionUri
    { _revisionUri     :: Uri
    , _revisionVersion :: Version
    } deriving (Eq, Show)

$(makeLenses ''RevisionUri)

data ArchiveUri = ArchiveUri
    { _archiveUri     :: Uri
    , _archiveVersion :: Version
    } deriving (Ord, Eq, Show)

instance SafeUri Uri where
    encodeUri Uri{..} = BS.intercalate "/"
        [ "http:/"
        , BS.concat [_uriHost, ":", BS.pack $ show _uriPort]
        , "cookbooks"
        , _uriCookbook
        ]

instance SafeUri Version where
    encodeUri (Version bs) = BS.map fn bs
      where
        fn '.' = '_'
        fn c   = c

instance SafeUri RevisionUri where
    encodeUri (RevisionUri uri ver) = appendVersion uri ver

instance SafeUri ArchiveUri where
    encodeUri (ArchiveUri uri ver) =
        BS.intercalate "/" [appendVersion uri ver, "archive"]

appendVersion :: Uri -> Version -> BS.ByteString
appendVersion uri ver =
    BS.intercalate "/" [encodeUri uri, "versions", encodeUri ver]

instance ToJSON Uri where
    toJSON = toJSON . encodeUri

instance ToJSON RevisionUri where
    toJSON = toJSON . encodeUri

instance ToJSON ArchiveUri where
    toJSON = toJSON . encodeUri

data Time = Time UnixTime deriving (Eq, Show)

instance IsString Time where
    fromString = Time . parseUnixTimeGMT timeFormat . BS.pack

instance ToJSON Time where
    toJSON (Time utime) = toJSON $ formatUnixTimeGMT timeFormat utime

instance FromJSON Time where
    parseJSON j = liftM (Time . parseUnixTimeGMT "%Y-%m-%dT%H:%M:%SZ") (parseJSON j)

timeFormat :: Format
timeFormat = "%Y-%m-%dT%H:%M:%SZ"

data Cookbook =
    Overview
    { name          :: Name
    , description   :: BS.ByteString
    , latestVersion :: Maybe RevisionUri
    , versions      :: [RevisionUri]
    , maintainer    :: User
    , createdAt     :: Time
    , updatedAt     :: Time
    }
 |  Revision
    { cookbook  :: OverviewUri
    , file      :: ArchiveUri
    , version   :: Version
    , createdAt :: Time
    , updatedAt :: Time
    } deriving (Eq, Show)

instance ToJSON Cookbook where
    toJSON Overview{..} = object
        [ "name"           .= name
        , "latest_version" .= latestVersion
        , "versions"       .= versions
        , "description"    .= description
        , "maintainer"     .= maintainer
        , "updated_at"     .= updatedAt
        , "created_at"     .= createdAt
        ]
    toJSON Revision{..} = object
        [ "cookbook"   .= cookbook
        , "file"       .= file
        , "version"    .= version
        , "updated_at" .= updatedAt
        , "created_at" .= createdAt
        ]
