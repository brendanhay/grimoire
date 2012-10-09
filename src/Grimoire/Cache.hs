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
    -- * Restricted Constructor
      Cache
    , newCache

    -- * Functions
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

newtype Cache k v = Cache (TMap k v)

newCache :: (MonadIO m, Ord k) => m (Cache k v)
newCache = liftIO . atomically $ Cache `liftM` (newTVar M.empty)

withCache :: (MonadIO m, Ord k) => Cache k v -> IO v -> k -> m v
withCache (Cache tvar) io key = do
    v <- lookup tvar key
    liftIO $ modifyMVar v cons
  where
    cons v = do
        y <- case v of
            Just x  -> return x
            Nothing -> io
        return (Just y, y)

--
-- Private
--

lookup :: (MonadIO m, Ord k) => TMap k v -> k -> m (MValue v)
lookup tvar key = liftIO $ do
    (m, v) <- atomically (readTVar tvar) >>= insert
    seq m . atomically $ writeTVar tvar m
    return v
  where
    insert m = case M.lookup key m of
        Just v  -> return (m, v)
        Nothing -> do
            v <- newMVar Nothing
            return (M.insert key v m, v)
