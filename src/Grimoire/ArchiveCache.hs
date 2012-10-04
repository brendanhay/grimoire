-- |
-- Module      : Grimoire.ArchiveCache
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.ArchiveCache (
    -- * Restricted Constructors
      ArchiveCache
    , empty

    -- * Functions
    , lookup
    ) where

import Prelude                 hiding (lookup)
import Control.Concurrent.MVar
import Control.Monad           (unless, liftM)
import Data.Conduit.Binary     (sinkFile)
import System.Directory        (doesFileExist, createDirectoryIfMissing)
import Grimoire.Types
import Grimoire.GitHub         (getTarball)

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M

type Lock   = MVar Bool
type LockDB = M.Map ArchiveUri Lock

data ArchiveCache = ArchiveCache
    { cacheDir   :: BS.ByteString
    , cacheAuth  :: Auth
    , cacheLocks :: MVar LockDB
    }

instance Show ArchiveCache where
    show = show . cacheDir

empty :: BS.ByteString -> Auth -> IO ArchiveCache
empty dir a = liftM (ArchiveCache dir a) (newMVar M.empty)

lookup :: ArchiveUri -> ArchiveCache -> IO FilePath
lookup uri@ArchiveUri{..} cache@ArchiveCache{..} = do
    p <- doesFileExist file
    unless p $ createDirectoryIfMissing True dir >> withLock uri cache req
    return file
  where
    req         = getTarball name _archiveVersion cacheAuth $ sinkFile file
    (dir, file) = paths name _archiveVersion cache
    name        = _uriCookbook _archiveUri

--
-- Private
--

paths :: Name -> Version -> ArchiveCache -> (FilePath, FilePath)
paths name ver ArchiveCache{..} = (BS.unpack dir, BS.unpack file)
  where
    join = BS.intercalate "/"
    dir  = join [cacheDir, name]
    file = join [dir, BS.concat [name, "-", encodeUri ver, ".tar.gz"]]

withLock :: ArchiveUri -> ArchiveCache -> IO () -> IO ()
withLock uri ArchiveCache{..} fn =
    lookupLock uri cacheLocks >>= flip modifyMVar_ f
  where
    f lock = fn >> return lock

lookupLock :: ArchiveUri -> MVar LockDB -> IO Lock
lookupLock uri db = modifyMVar db f
  where
    f db' = case M.lookup uri db' of
        Just lock -> return (db', lock)
        Nothing   -> addLock uri db'

addLock :: ArchiveUri -> LockDB -> IO (LockDB, Lock)
addLock uri db = do
   lock <- newMVar True
   return (M.insert uri lock db, lock)
