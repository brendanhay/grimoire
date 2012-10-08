-- |
-- Module      : Grimoire.Cache
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.Cache (
    -- * Restricted Constructors
      Cache
    , empty
    , withCache
    ) where

import Prelude                hiding (lookup)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map as M

type Lock v        = MVar (Maybe v)
type LockStore k v = TVar (M.Map k (Lock v))

data Cache k v = Cache
    { _store :: LockStore k v
    }

-- type Cache k v = ReaderT (Cache k v) IO

empty :: (MonadIO m, Eq k, Ord k) => m (Cache k v)
empty = liftIO . atomically $ do
    store <- newTVar M.empty
    return $ Cache store

withCache :: (MonadIO m, Eq k, Ord k) => Cache k v -> IO v -> k -> m v
withCache cache = withStore (_store cache)

--
-- Private
--

withStore :: (MonadIO m, Ord k) => LockStore k v -> IO v -> k -> m v
withStore store io key = findLock store key >>= liftIO . flip modifyMVar lookup
  where
    lookup lock = do
        val <- case lock of
            Just v  -> return v
            Nothing -> io
        return (Just val, val)

findLock :: (MonadIO m, Ord k) => LockStore k v -> k -> m (Lock v)
findLock store key = liftIO $ do
    (locks', lock) <- atomically (readTVar store) >>= find
    seq locks' . atomically $ writeTVar store locks'
    return lock
  where
    find ls = case M.lookup key ls of
        Just l ->
            return (ls, l)
        Nothing -> do
            l <- newMVar Nothing
            return (M.insert key l ls, l)
