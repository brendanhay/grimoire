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
    -- * Aliases
      Name
    , File

    -- * Auth
    , Org
    , User
    , Password
    , Auth(..)

    , orgStr
    , userStr
    , passStr

    , authOrg
    , authUser
    , authPass

    -- * Versioning
    , Version

    , versionStr

    -- * Uri
    , SafeUri(..)
    , Uri(..)
    , OverviewUri(..)
    , RevisionUri(..)
    , ArchiveUri(..)

    -- * Time
    , Time(..)

    -- * Cookbooks
    , Overview(..)
    , Revision(..)

    -- * Config
    , BaseUri
    , Config(..)

    , auth
    , cacheDir
    , baseUri
    ) where

import Control.Monad        (liftM)
import Data.Aeson           (ToJSON(..), FromJSON(..), object, (.=))
import Data.Default
import Data.Function        (on)
import Control.Lens  hiding ((.=))
import Data.Monoid
import Data.String          (IsString(..))
import Data.UnixTime        (UnixTime, Format, parseUnixTimeGMT, formatUnixTimeGMT)

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BL

type Name = BS.ByteString
type File = BL.ByteString

--
-- Auth
--

newtype Org = Org BS.ByteString
    deriving (Show, Eq)

instance Default Org where
    def = "organisation"

instance IsString Org where
    fromString = Org . fromString

orgStr :: Auth -> BS.ByteString
orgStr = (\(Org bs) -> bs) . _authOrg

newtype User = User Name
    deriving (Show, Eq)

instance Default User where
    def = "user"

instance IsString User where
    fromString = User . fromString

instance ToJSON User where
    toJSON (User bs) = toJSON bs

instance FromJSON User where
    parseJSON j = liftM User (parseJSON j)

userStr :: Auth -> BS.ByteString
userStr = (\(User bs) -> bs) . _authUser

newtype Password = Password BS.ByteString
    deriving (Eq)

instance Show Password where
    show _ = "Password \"*****\""

instance Default Password where
    def = "password"

instance IsString Password where
    fromString = Password . fromString

passStr :: Auth -> BS.ByteString
passStr = (\(Password bs) -> bs) . _authPass

data Auth = Auth
    { _authOrg  :: Org
    , _authUser :: User
    , _authPass :: Password
    } deriving (Eq, Show)

$(makeLenses ''Auth)

instance Default Auth where
    def = Auth def def def

instance Monoid Auth where
    mempty      = Auth def def def
    mappend a b = a
        { _authOrg  = f def _authOrg
        , _authUser = rappend def (_authUser a) (_authUser b)
        , _authPass = f def _authPass
        }
      where
        f d g = (rappend d `on` g) a b

rappend :: Eq a => a -> a -> a -> a
rappend d x y | x == d    = y
              | y == d    = x
              | otherwise = y

--
-- Versioning
--

data Version = Version BS.ByteString
    deriving (Ord, Eq, Show)

instance SafeUri Version where
    encodeUri (Version bs) = BS.map fn bs
      where
        fn '.' = '_'
        fn c   = c

instance IsString Version where
    fromString = Version . BS.pack

instance ToJSON Version where
    toJSON = toJSON . versionStr

versionStr :: Version -> BS.ByteString
versionStr (Version bs) = BS.map fn bs
  where
    fn '_' = '.'
    fn c   = c

--
-- Uri
--

class SafeUri a where
    encodeUri :: a -> BS.ByteString

data Uri = Uri
    { _uriHost     :: BS.ByteString
    , _uriPort     :: Int
    , _uriCookbook :: Name
    } deriving (Ord, Eq, Show)

instance Default Uri where
    def = Uri "localhost" 5000 ""

instance SafeUri Uri where
    encodeUri Uri{..} = BS.intercalate "/"
        [ "http:/"
        , BS.concat [_uriHost, ":", BS.pack $ show _uriPort]
        , "cookbooks"
        , _uriCookbook
        ]

instance ToJSON Uri where
    toJSON = toJSON . encodeUri

newtype OverviewUri = OverviewUri Uri
    deriving (Eq, Show)

instance SafeUri OverviewUri where
    encodeUri (OverviewUri uri) = encodeUri uri

instance ToJSON OverviewUri where
    toJSON = toJSON . encodeUri

data RevisionUri = RevisionUri
    { _revisionUri     :: Uri
    , _revisionVersion :: Version
    } deriving (Eq, Show)

instance SafeUri RevisionUri where
    encodeUri (RevisionUri uri ver) = appendVersion uri ver

appendVersion :: Uri -> Version -> BS.ByteString
appendVersion uri ver =
    BS.intercalate "/" [encodeUri uri, "versions", encodeUri ver]

instance ToJSON RevisionUri where
    toJSON = toJSON . encodeUri

data ArchiveUri = ArchiveUri
    { _archiveUri     :: Uri
    , _archiveVersion :: Version
    } deriving (Ord, Eq, Show)

instance SafeUri ArchiveUri where
    encodeUri (ArchiveUri uri ver) =
        BS.intercalate "/" [appendVersion uri ver, "archive"]

instance ToJSON ArchiveUri where
    toJSON = toJSON . encodeUri

--
-- Time
--

data Time = Time UnixTime deriving (Eq, Show)

instance IsString Time where
    fromString = Time . parseUnixTimeGMT timeFormat . BS.pack

instance ToJSON Time where
    toJSON (Time utime) = toJSON $ formatUnixTimeGMT timeFormat utime

instance FromJSON Time where
    parseJSON j = liftM (Time . parseUnixTimeGMT "%Y-%m-%dT%H:%M:%SZ") (parseJSON j)

timeFormat :: Format
timeFormat = "%Y-%m-%dT%H:%M:%SZ"

--
-- Cookbooks
--

data Overview = Overview
    { ovName        :: Name
    , ovDescription :: BS.ByteString
    , ovLatest      :: Maybe RevisionUri
    , ovVersions    :: [RevisionUri]
    , ovMaintainer  :: User
    , ovCreated     :: Time
    , ovUpdated     :: Time
    } deriving (Eq, Show)

instance ToJSON Overview where
    toJSON Overview{..} = object
        [ "name"           .= ovName
        , "latest_version" .= ovLatest
        , "versions"       .= ovVersions
        , "description"    .= ovDescription
        , "maintainer"     .= ovMaintainer
        , "updated_at"     .= ovUpdated
        , "created_at"     .= ovCreated
        ]

data Revision = Revision
    { revCookbook :: OverviewUri
    , revFile     :: ArchiveUri
    , revVersion  :: Version
    , revCreated  :: Time
    , revUpdated  :: Time
    } deriving (Eq, Show)

instance ToJSON Revision where
    toJSON Revision{..} = object
        [ "cookbook"   .= revCookbook
        , "file"       .= revFile
        , "version"    .= revVersion
        , "updated_at" .= revUpdated
        , "created_at" .= revCreated
        ]

--
-- Config
--

type BaseUri = Name -> Uri

instance Eq BaseUri where
    a == b = a "" == b ""

instance Show BaseUri where
    show u = "BaseUri \"" ++ (BS.unpack . encodeUri $ u "<cookbook>\"")

data Config = Config
    { _auth      :: Auth
    , _cacheDir  :: BS.ByteString
    , _baseUri   :: BaseUri
    }

$(makeLenses ''Config)

instance Monoid Config where
    mempty      = Config mempty ".cache" def
    mappend a b = a
        { _auth     = (mappend `on` _auth) a b
        , _cacheDir = f ".cache" _cacheDir
        , _baseUri  = f def _baseUri
        }
      where
        f d g = (rappend d `on` g) a b

instance Show Config where
    show Config{..} = unlines [ "Grimoire:"
                              , "org: "   ++ org
                              , "user: "  ++ user
                              , "pass: "  ++ pass
                              , "cache: " ++ cache
                              , "uri: "   ++ uri
                              ]
      where
        org   = show $ _authOrg _auth
        user  = show $ _authUser _auth
        pass  = show $ _authPass _auth
        cache = show $ BS.concat ["./" `BS.append` _cacheDir, "/<cookbook>"]
        uri   = show $ _baseUri
