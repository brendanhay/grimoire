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
import Snap.Http.Server  (httpServe, getOther)
import System.IO
import Grimoire.Handlers (site)
import Grimoire.Config   (parseConfig)

main :: IO ()
main = do
  -- Just for development to ensure foreman flushes stdout
  hSetBuffering stdout NoBuffering
  conf <- parseConfig
  print conf
  httpServe conf $ site conf

