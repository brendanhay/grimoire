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
import Control.Monad                 (liftM)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Control.Concurrent
import Control.Concurrent.STM

import qualified Data.Map as M

type MValue v = MVar (Maybe v)
type TMap k v = TVar (M.Map k (MValue v))

newtype Cache k v  = Cache (TMap k v)

newCache :: (MonadIO m, Ord k) => m (Cache k v)
newCache = liftIO . atomically $ liftM Cache (newTVar M.empty)

withCache :: (MonadIO m, Ord k) => Cache k v -> IO v -> k -> m v
withCache (Cache tvar) io key = do
    lock <- findMValue tvar key
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

findMValue :: (MonadIO m, Ord k) => TMap k v -> k -> m (MValue v)
findMValue tvar key = liftIO $ do
    (locks, lock) <- atomically (readTVar tvar) >>= lookup
    seq locks . atomically $ writeTVar tvar locks
    return lock
  where
    lookup ls = case M.lookup key ls of
        Just l ->
            return (ls, l)
        Nothing -> do
            l <- newMVar Nothing
            return (M.insert key l ls, l)
