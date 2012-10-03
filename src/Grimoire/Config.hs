-- |
-- Module      : Grimoire.Config
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Grimoire.Config (
    -- * Functions
      parseConfig
    ) where

import Control.Lens          ((.~))
import Data.Monoid
import Data.Maybe            (fromMaybe)
import Snap.Core             (MonadSnap)
import Snap.Http.Server
import System.Console.GetOpt
import Grimoire.Types

import qualified Data.ByteString.Char8 as BS

parseConfig :: MonadSnap m => IO (Config m AppConfig)
parseConfig = extendedCommandLineConfig flags' mappend defaults
  where
    defaults = emptyConfig
    flags'   = flags (fromMaybe mempty $ getOther defaults) ++ optDescrs defaults

--
-- Private
--

flags :: AppConfig -> [OptDescr (Maybe (Config m AppConfig))]
flags conf@AppConfig{..} = map (fmapOpt $ fmap (`setOther` mempty))
    [ Option [] ["github-org"]
          (ReqArg (upd authOrg . BS.pack) "ORG")
          $ "github org" ++ text _authOrg
    , Option [] ["github-user"]
          (ReqArg (upd authUser . BS.pack) "USER")
          $ "github user" ++ text _authUser
    , Option [] ["github-pass"]
          (ReqArg (upd authPass . BS.pack) "PASS")
          $ "github password" ++ text _authPass
    ]
  where
    upd l v = Just $ (auth . l .~ v) conf
    text f  = (", default " ++) . show $ f _auth
