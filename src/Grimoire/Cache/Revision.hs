-- |
-- Module      : Grimoire.Cache.Revision
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.Cache.Revision (
    -- * Exported Types
      Cache

    -- * Restricted Constructors
    , new
    ) where

import Prelude                hiding (lookup)
import Control.Monad                 (liftM)
import Control.Monad.IO.Class        (MonadIO, liftIO)
import Grimoire.GitHub               (getRevision)
import Grimoire.Types

import qualified Grimoire.Cache as C

type Key   = (Name, Version)
type Cache = Cache_ Key Revision

data Cache_ k v = Cache
    { _cache :: C.STMCache k v
    }

instance C.Cache Cache_ Key Revision where
    lookup key (Cache cache) = C.lookup key cache

new :: MonadIO io => AppConfig -> io Cache
new conf = liftM Cache (C.newSTM ctor)
  where
    ctor (name, ver) = liftIO $ getRevision name ver conf
