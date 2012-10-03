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
    , Time(..)
    , Cookbook(..)

    -- * Restricted Constructors
    , Version

    -- * Lenses
    , auth
    , authOrg
    , authUser
    , authPass

    -- * Accessors
    , org
    , user
    , pass
    ) where

import Control.Monad (liftM)
import Data.Aeson    (ToJSON(..), FromJSON(..), object, (.=))
import Data.Function (on)
import Control.Lens hiding ((.=))
import Data.Monoid
import Data.String   (IsString(..))
import Data.UnixTime (UnixTime, Format, parseUnixTimeGMT, formatUnixTimeGMT)

import qualified Data.ByteString.Char8 as BS

type Name = BS.ByteString
type File = BS.ByteString

data Org = Org BS.ByteString
    deriving (Show, Eq)

instance Monoid Org where
    mempty  = "organisation"
    mappend = mappend'

instance IsString Org where
    fromString = Org . fromString

data User = User Name
    deriving (Show, Eq)

instance Monoid User where
    mempty  = "user"
    mappend = mappend'

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
    mappend = mappend'

mappend' :: (Monoid m, Eq m) => m -> m -> m
mappend' a b | a == mempty = b
             | b == mempty = a
             | otherwise   = b

instance IsString Password where
    fromString = Password . fromString

data Auth = Auth
    { _authOrg  :: Org
    , _authUser :: User
    , _authPass :: Password
    } deriving (Show)

$(makeLenses ''Auth)

org, user, pass :: Auth -> BS.ByteString
org  Auth{..} = o where Org o      = _authOrg
user Auth{..} = u where User u     = _authUser
pass Auth{..} = p where Password p = _authPass

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
    { _auth :: Auth
    } deriving (Show)

$(makeLenses ''AppConfig)

instance Monoid AppConfig where
    mempty      = AppConfig mempty
    mappend a b = AppConfig
        { _auth = mappend (_auth a) (_auth b)
        }

data Version = Version
    { versionStr :: BS.ByteString
    } deriving (Eq, Show)

instance IsString Version where
    fromString s = Version (BS.map fn $ BS.pack s)
      where
        fn '.' = '_'
        fn c   = c

instance ToJSON Version where
    toJSON (Version s) = toJSON $ BS.map fn s
      where
        fn '_' = '.'
        fn c   = c

data Uri = Uri
    { uriHost     :: BS.ByteString
    , uriPort     :: Int
    , uriCookbook :: Name
    , uriVersion  :: Version
    } deriving (Eq, Show)

instance ToJSON Uri where
    toJSON Uri{..} = toJSON $ BS.intercalate "/" lst
      where
         lst = [ "http:/"
               , BS.concat [uriHost, ":", BS.pack $ show uriPort]
               , "cookbooks"
               , uriCookbook
               , "versions"
               , versionStr uriVersion
               ]

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
    , latestVersion :: Maybe Uri
    , versions      :: [Uri]
    , maintainer    :: User
    , createdAt     :: Time
    , updatedAt     :: Time
    }
 |  Revision
    { cookbook  :: Uri
    , file      :: File
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
