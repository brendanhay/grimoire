{-# LANGUAGE DoAndIfThenElse #-}

-- |
-- Module      : Grimoire.Cache.Archive
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.Cache.Archive (
    -- * Exported Types
      Cache

    -- * Restricted Constructors
    , new
    ) where

import Prelude        hiding (lookup)
import Control.Monad         (liftM)
import Data.Conduit.Binary   (sinkFile)
import System.Directory      (doesFileExist, createDirectoryIfMissing)
import Grimoire.Types hiding (auth)
import Grimoire.GitHub       (getTarball)

import qualified Data.ByteString.Char8 as BS
import qualified Grimoire.Cache        as C

type Cache = Cache_ ArchiveUri FilePath

data Cache_ k v = Cache
    { _dir   :: BS.ByteString
    , _cache :: C.AtomicCache k v
    }

instance C.Cache Cache_ ArchiveUri FilePath where
    lookup uri@ArchiveUri{..} Cache{..} = do
         p <- doesFileExist file
         if p then return file
         else createDirectoryIfMissing True dir >> C.lookup uri _cache
      where
        name = _uriCookbook _archiveUri
        (dir, file) = paths name _archiveVersion _dir

new :: AppConfig -> IO Cache
new conf = liftM (Cache base) (C.atomically $ retrieve conf base)
  where
    base = _cacheDir conf

--
-- Private
--

retrieve :: AppConfig -> BS.ByteString -> ArchiveUri -> IO FilePath
retrieve conf base ArchiveUri{..} = do
    getTarball name _archiveVersion conf $ sinkFile file
    return file
  where
    name      = _uriCookbook _archiveUri
    (_, file) = paths name _archiveVersion base

paths :: Name -> Version -> BS.ByteString -> (FilePath, FilePath)
paths name ver dir = (BS.unpack pref, BS.unpack file)
  where
    join = BS.intercalate "/"
    pref = join [dir, name]
    file = join [pref, BS.concat [name, "-", encodeUri ver, ".tar.gz"]]

