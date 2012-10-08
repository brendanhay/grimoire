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
    -- * Type Class
      Cache
    , lookup

    -- * Restricted Constructors
    , STMCache
    , withSTM
    ) where

import Prelude                hiding (lookup)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map as M

class Ord k => Cache c k v where
    lookup :: (Ord k) => k -> c k v -> IO v

type Ctor k v = k -> IO v

type Lock  v = Maybe v
type MLock v = MVar (Lock v)

type LockStore  k v = M.Map k (MLock v)
type TLockStore k v = TVar (LockStore k v)

data STMCache k v = STMCache
    { _store :: TLockStore k v
    , _ctor  :: k -> IO v
    }

instance Ord k => Cache STMCache k v where
    lookup key STMCache{..} = withStore key _store _ctor

newSTM :: MonadIO io => Ctor k v -> io (STMCache k v)
newSTM ctor = liftIO . atomically $ do
    store <- newTVar M.empty
    return $ STMCache store ctor

--
-- Private
--

withStore :: (MonadIO io, Ord k) => k -> TLockStore k v -> Ctor k v -> io v
withStore key store ctor = do
    lock <- findLock key store
    liftIO $ modifyMVar lock (\v -> do val <- mcons v; return (Just val, val))
  where
    mcons Nothing  = ctor key
    mcons (Just v) = return v

findLock :: (MonadIO io, Ord k) => k -> TLockStore k v -> io (MLock v)
findLock key store = liftIO $ do
    locks          <- atomically $ readTVar store
    (locks', lock) <- find locks
    seq locks' . atomically $ writeTVar store locks'
    return lock
  where
    find ls = case M.lookup key ls of
        Just l ->
            return (ls, l)
        Nothing -> do
            l <- newMVar Nothing
            return (M.insert key l ls, l)
