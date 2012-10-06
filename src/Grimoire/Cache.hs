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
    , force

    -- * Restricted Constructors
    , SharedCache
    , shared
    ) where

import Prelude hiding (lookup)
import Control.Concurrent

import qualified Data.Map as M

class Ord k => Cache c k v where
    lookup :: (Ord k) => k -> c k v -> IO v
    force  :: (Ord k) => k -> c k v -> IO v

type Lock v        = MVar (Maybe v)
type LockStore k v = MVar (M.Map k (Lock v))

data SharedCache k v = SharedCache
    { _store :: LockStore k v
    , _ctor  :: k -> IO v
    }

instance Ord k => Cache SharedCache k v where
    lookup key SharedCache{..} = withKey key _store _ctor
    force = lookup

shared :: (k -> IO v) -> IO (SharedCache k v)
shared ctor = do
    store <- newMVar M.empty
    return $ SharedCache store ctor

--
-- Private
--

withKey :: (Ord k) => k -> LockStore k v -> (k -> IO v) -> IO v
withKey key store fn = do
    lock <- withStore key store
    modifyMVar lock (\v -> do val <- mcons v; return (Just val, val))
  where
    mcons Nothing  = fn key
    mcons (Just v) = return v

withStore :: (Ord k) => k -> LockStore k v -> IO (Lock v)
withStore key = flip modifyMVar f
  where
    f store = case M.lookup key store of
        Just lock ->
            return (store, lock)
        Nothing -> do
            lock <- newMVar Nothing
            return (M.insert key lock store, lock)
