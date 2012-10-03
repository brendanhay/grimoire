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
      Org
    , UserName
    , Password
    , Name
    , File
    , Auth(..)
    , AppConfig(..)
    , Uri(..)
    , Time(..)
    , Cookbook(..)

    -- * Restricted Constructors
    , Version
    ) where

import Control.Monad (liftM)
import Data.Aeson    (ToJSON(..), FromJSON(..), object, (.=))
import Data.Function (on)
import Data.Monoid
import Data.String   (IsString(..))
import Data.Text     (Text)
import Data.UnixTime (UnixTime, Format, parseUnixTimeGMT, formatUnixTimeGMT)

import qualified Data.ByteString.Char8 as BS

type Org      = BS.ByteString
type UserName = BS.ByteString
type Password = BS.ByteString
type Name     = BS.ByteString
type File     = BS.ByteString

data Auth = Auth
    { authOrg  :: Maybe Org
    , authUser :: Maybe UserName
    , authPass :: Maybe Password
    } deriving (Show)

instance Monoid Auth where
    mempty      = Auth Nothing Nothing Nothing
    mappend a b = Auth
        { authOrg  = ov authOrg
        , authUser = ov authUser
        , authPass = ov authPass
        }
      where
        ov f = getLast $! (mappend `on` (Last . f)) a b

data AppConfig = AppConfig
    { auth :: Auth
    } deriving (Show)

instance Monoid AppConfig where
    mempty      = AppConfig mempty
    mappend a b = AppConfig
        { auth = mappend (auth a) (auth b)
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

data Cookbook =
    Overview
    { name          :: Name
    , description   :: BS.ByteString
    , latestVersion :: Maybe Uri
    , versions      :: [Uri]
    , maintainer    :: UserName
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

--
-- Private
--

timeFormat :: Format
timeFormat = "%Y-%m-%dT%H:%M:%SZ"
