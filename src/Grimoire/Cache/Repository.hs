-- |
-- Module      : Grimoire.Cache.Repository
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.Cache.Repository (
    -- * Exported Types
      Cache

    -- * Restricted Constructors
    , new
    ) where

import Prelude         hiding (lookup)
import Control.Monad          (liftM)
import Data.Maybe             (fromJust)
import Grimoire.Types         (Auth, Name)
import Grimoire.GitHub        (Repository, getRepository)

import qualified Grimoire.Cache as C

type Cache = Cache_ Name Repository

data Cache_ k v = Cache
    { _backing :: C.SharedCache k v
    }

instance C.Cache Cache_ Name Repository where
    lookup name (Cache db) = C.lookup name db
    force  name (Cache db) = C.force name db

new :: Auth -> IO Cache
new auth = liftM Cache (C.shared ctor)
  where
    ctor name = getRepository name auth >>= return . fromJust
