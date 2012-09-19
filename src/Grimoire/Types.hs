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
    , Version(..)
    , Uri(..)
    , Time(..)
    , Cookbook(..)
    ) where

import Data.Aeson    (ToJSON(..), object, (.=))
import Data.List     (intercalate)
import Data.String   (IsString(..))
import Data.UnixTime (UnixTime, Format, parseUnixTimeGMT, formatUnixTimeGMT)
import Text.Regex    (mkRegex, subRegex)

import qualified Data.ByteString.Char8 as BS

type Name = String
type File = String

data Version = Version String deriving (Eq, Show)

instance IsString Version where
    fromString str = Version $ subRegex (mkRegex "\\.") str "_"

instance ToJSON Version where
    toJSON (Version str) = toJSON $ subRegex (mkRegex "_") str "."

data Uri = Uri Name Version deriving (Eq, Show)

instance ToJSON Uri where
    toJSON (Uri name (Version version)) = toJSON $ intercalate "/" lst
      where
         lst = [ "http://localhost:7000/cookbooks"
               , name
               , "versions"
               , version
               ]

data Time = Time UnixTime deriving (Eq, Show)

instance IsString Time where
    fromString = Time . parseUnixTimeGMT timeFormat . BS.pack

instance ToJSON Time where
    toJSON (Time utime) = toJSON $ formatUnixTimeGMT timeFormat utime

data Cookbook =
    Overview
    { name          :: Name
    , latestVersion :: Uri
    , versions      :: [Uri]
    , createdAt     :: Time
    , updatedAt     :: Time
    }
  | Specific
    { cookbook  :: Uri
    , file      :: File
    , version   :: Version
    , createdAt :: Time
    , updatedAt :: Time
    }
  deriving (Eq, Show)

instance ToJSON Cookbook where
    toJSON Overview{..} = object
        [ "name"              .= name
        , "latest_version"    .= latestVersion
        , "versions"          .= versions
        , "description"       .= ("description" :: String)
        , "external_url"      .= ("github.com/soundcloud/system" :: String)
        , "maintainer"        .= ("brendan" :: String)
        , "category"          .= ("stuff" :: String)
        , "average_rating"    .= ("5" :: String)
        , "updated_at"        .= updatedAt
        , "created_at"        .= createdAt
        ]
    toJSON Specific{..} = object
        [ "cookbook"          .= cookbook
        , "file"              .= file
        , "tarball_file_size" .= ("45248" :: String)
        , "version"           .= version
        , "license"           .= ("Apache 2.0" :: String)
        , "average_rating"    .= ("null" :: String)
        , "updated_at"        .= updatedAt
        , "created_at"        .= createdAt
        ]

--
-- Private
--

timeFormat :: Format
timeFormat = "%Y-%m-%dT%H:%M:%SZ"
