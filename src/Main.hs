-- |
-- Module      : Main
-- Copyright   : (c) 2012 Brendan Hay <brendan@soundcloud.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan@soundcloud.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--

module Main (
    -- * Main Entry Point
      main
    ) where

import Data.Maybe        (fromJust)
import Snap.Http.Server
import System.IO
import Grimoire.Handlers (site)
import Grimoire.Config   (parseConfig)
import Grimoire.Types

import qualified Grimoire.Cache.Archive    as A
import qualified Grimoire.Cache.Repository as R

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering -- Ensure foreman flushes stdout
    app <- parseConfig
    print app

    let conf@AppConfig{..} = fromJust $ getOther app

    repos <- R.new _auth
    arcs  <- A.new _auth _cacheDir

    httpServe app $ (site conf repos arcs)
