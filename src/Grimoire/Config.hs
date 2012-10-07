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
    -- * Exported Types
      AppConfig(..)

    -- * Functions
    , parseConfig
    ) where

import Control.Lens          ((.~))
import Control.Monad         (liftM)
import Data.Monoid
import Data.Maybe            (fromJust, fromMaybe)
import Data.String
import Snap.Core             (MonadSnap)
import Snap.Http.Server
import System.Console.GetOpt
import Grimoire.Types

import qualified Data.ByteString.Char8    as BS

parseConfig :: MonadSnap m => IO (Config m AppConfig)
parseConfig = liftM initConfig (extendedCommandLineConfig flags' mappend empty)
  where
    empty  = emptyConfig
    flags' = flags (fromMaybe mempty $ getOther empty) ++ optDescrs empty

--
-- Private
--

initConfig :: Config m AppConfig -> Config m AppConfig
initConfig conf = setOther app conf
  where
    f g = fromJust $ g conf
    app = (f getOther)
        { _baseUri = Uri (f getHostname) (f getPort)
        }

flags :: AppConfig -> [OptDescr (Maybe (Config m AppConfig))]
flags conf@AppConfig{..} = map (fmapOpt $ fmap (`setOther` mempty))
    [ Option [] ["github-org"] (ReqArg (upd authOrg) "ORG")
          $ "github org" ++ text _authOrg
    , Option [] ["github-user"] (ReqArg (upd authUser) "USER")
          $ "github user" ++ text _authUser
    , Option [] ["github-pass"] (ReqArg (upd authPass) "PASS")
          $ "github password" ++ text _authPass
    , Option [] ["cache-dir"] (ReqArg (\s -> Just $ conf { _cacheDir = BS.pack s }) "DIR")
          $ "cache directory, default " ++ show _cacheDir
    ]
  where
    upd l s = Just $ (auth . l .~ fromString s) conf
    text f  = (", default " ++) . show $ f _auth
