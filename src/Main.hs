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

import qualified Grimoire.Cache as C

main :: IO ()
main = do
    -- Ensure foreman flushes stdout
    hSetBuffering stdout NoBuffering

    -- Get some command line args
    httpConf <- parseConfig
    print httpConf

    -- Extract the important shit
    let appConf = fromJust $ getOther httpConf
    print appConf

    -- Setup type caches
    revs <- C.empty
    tars <- C.empty

    -- Start the serve with the site handlers
    httpServe httpConf $ site appConf revs tars
