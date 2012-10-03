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
          (ReqArg (\s -> Just $ conf { auth = mempty { authOrg = Just $ BS.pack s } }) "ORG")
          $ "github org" ++ text authOrg
    , Option [] ["github-user"]
          (ReqArg (\s -> Just $ conf { auth = mempty { authUser = Just $ BS.pack s } }) "USER")
          $ "github user" ++ text authUser
    , Option [] ["github-pass"]
          (ReqArg (\s -> Just $ conf { auth = mempty { authPass = Just $ BS.pack s } }) "PASS")
          $ "github pass" ++ text authPass
    ]
  where
    text f = maybe "" ((", default " ++) . show) $ f auth


flags :: AppConfig -> [OptDescr (Maybe (Config m AppConfig))]
flags conf@AppConfig{..} = map (fmapOpt $ fmap (`setOther` mempty))
    [ Option [] ["github-org"]
          (ReqArg (\s -> Just $ conf { auth = mempty { authOrg = Just $ BS.pack s } }) "ORG")
          $ "github org" ++ text authOrg
    , Option [] ["github-user"]
          (ReqArg (\s -> Just $ conf { auth = mempty { authUser = Just $ BS.pack s } }) "USER")
          $ "github user" ++ text authUser
    , Option [] ["github-pass"]
          (ReqArg (\s -> Just $ conf { auth = mempty { authPass = Just $ BS.pack s } }) "PASS")
          $ "github pass" ++ text authPass
    ]
  where
    text f = maybe "" ((", default " ++) . show) $ f auth
