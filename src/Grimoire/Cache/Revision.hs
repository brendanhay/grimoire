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

import Prelude         hiding (lookup)
import Control.Monad          (liftM)
import Grimoire.Types
import Grimoire.GitHub        (getRevision)

import qualified Grimoire.Cache as C

type Key   = (Name, Version)
type Cache = Cache_ Key Revision

data Cache_ k v = Cache
    { _backing :: C.SharedCache k v
    }

instance C.Cache Cache_ Key Revision where
    lookup key (Cache db) = C.lookup key db
    force  key (Cache db) = C.force key db

new :: AppConfig -> IO Cache
new conf = liftM Cache (C.shared $ \(name, ver) -> getRevision name ver conf)