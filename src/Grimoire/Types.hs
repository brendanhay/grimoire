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
    , OverviewUri(..)
    , RevisionUri(..)
    , ArchiveUri(..)
    , Time(..)
    , Cookbook(..)
    , Version(..)

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
import Control.Lens hiding ((.=))
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
    toJSON (User s) = toJSON s

instance FromJSON User where
    parseJSON j = parseJSON j >>= return . User

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
    } deriving (Show)

$(makeLenses ''Auth)

orgStr, userStr, passStr :: Auth -> BS.ByteString
orgStr  Auth{..} = o where Org o      = _authOrg
userStr Auth{..} = u where User u     = _authUser
passStr Auth{..} = p where Password p = _authPass

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
    } deriving (Show)

$(makeLenses ''AppConfig)

instance Monoid AppConfig where
    mempty      = AppConfig mempty ".cache"
    mappend a b = AppConfig
        { _auth     = mappend (_auth a) (_auth b)
        , _cacheDir = rappend (_cacheDir a) (_cacheDir b)
        }

data Version = Version
    { versionStr :: BS.ByteString
    } deriving (Eq, Show)

instance IsString Version where
    fromString = Version . BS.pack

instance ToJSON Version where
    toJSON (Version s) = toJSON $ BS.map fn s
      where
        fn '_' = '.'
        fn c   = c

class SafeUri a where
    fromUri :: a -> BS.ByteString

data Uri = Uri
    { uriHost     :: BS.ByteString
    , uriPort     :: Int
    , uriCookbook :: Name
    } deriving (Eq, Show)

type OverviewUri = Uri

data RevisionUri = RevisionUri Uri Version deriving (Eq, Show)
data ArchiveUri  = ArchiveUri Uri Version  deriving (Eq, Show)

instance SafeUri Uri where
    fromUri Uri{..} = BS.intercalate "/"
        [ "http:/"
        , BS.concat [uriHost, ":", BS.pack $ show uriPort]
        , "cookbooks"
        , uriCookbook
        ]

instance SafeUri Version where
    fromUri (Version s) = BS.map fn s
      where
        fn '.' = '_'
        fn c   = c

instance SafeUri RevisionUri where
    fromUri (RevisionUri u v) = appendVersion u v

instance SafeUri ArchiveUri where
    fromUri (ArchiveUri u v) = BS.intercalate "/" [appendVersion u v, "archive"]

appendVersion :: Uri -> Version -> BS.ByteString
appendVersion u v = BS.intercalate "/" [fromUri u, "versions", fromUri v]

instance ToJSON Uri where
    toJSON = toJSON . fromUri

instance ToJSON RevisionUri where
    toJSON = toJSON . fromUri

instance ToJSON ArchiveUri where
    toJSON = toJSON . fromUri

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
