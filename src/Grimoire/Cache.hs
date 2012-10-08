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
    , newCache
    , withCache
    ) where

import Prelude                hiding (lookup)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map as M

type Lock v        = MVar (Maybe v)
type LockStore k v = TVar (M.Map k (Lock v))

newtype Cache k v  = Cache (LockStore k v)

instance Show (Cache k v) where
    show _ = "<#Cache>"

newCache :: (MonadIO m, Eq k, Ord k) => m (Cache k v)
newCache = liftIO . atomically $ do
    store <- newTVar M.empty
    return $ Cache store

withCache :: (MonadIO m, Eq k, Ord k) => Cache k v -> IO v -> k -> m v
withCache (Cache store) io key = do
    lock <- findLock store key
    liftIO $ modifyMVar lock lookup
  where
    lookup l = do
        y <- case l of
            Just x  -> return x
            Nothing -> io
        return (Just y, y)

--
-- Private
--

findLock :: (MonadIO m, Ord k) => LockStore k v -> k -> m (Lock v)
findLock store key = liftIO $ do
    (locks, lock) <- atomically (readTVar store) >>= lookup
    seq locks . atomically $ writeTVar store locks
    return lock
  where
    lookup ls = case M.lookup key ls of
        Just l ->
            return (ls, l)
        Nothing -> do
            l <- newMVar Nothing
            return (M.insert key l ls, l)
