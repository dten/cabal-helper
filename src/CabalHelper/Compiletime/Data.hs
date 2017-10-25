-- Copyright (C) 2015,2017  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}

{-|
Module      : CabalHelper.Compiletime.Data
Description : Embeds source code for runtime component using TH
License     : AGPL-3
-}

module CabalHelper.Compiletime.Data where

import Control.Monad
import Control.Monad.IO.Class
import Data.Functor
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.ByteString as BS
import qualified Data.ByteString.UTF8 as UTF8
import Language.Haskell.TH
import System.Directory
import System.FilePath
import System.IO.Temp
import Prelude

import CabalHelper.Compiletime.Compat.Environment

withSystemTempDirectoryEnv :: String -> (FilePath -> IO b) -> IO b
withSystemTempDirectoryEnv tpl f = do
  m <- liftIO $ lookupEnv "CABAL_HELPER_KEEP_SOURCEDIR"
  case m of
    Nothing -> withSystemTempDirectory tpl f
    Just _  -> do
           tmpdir <- getCanonicalTemporaryDirectory
           f =<< createTempDirectory tmpdir tpl

withHelperSources :: Maybe FilePath -> (FilePath -> IO a) -> IO a
withHelperSources mdir action = withDir mdir $ \dir -> do
    let chdir = dir </> "CabalHelper"
    liftIO $ do
      createDirectoryIfMissing True $ chdir </> "Runtime"
      createDirectoryIfMissing True $ chdir </> "Shared"

    let modtime = read
          -- See https://reproducible-builds.org/specs/source-date-epoch/
          $(runIO $ do
             msde <- lookupEnv "SOURCE_DATE_EPOCH"
             let parse :: String -> POSIXTime
                 parse = fromInteger . read
             utctime <- getCurrentTime
             return $ LitE . StringL $ show $
                 maybe utctime (posixSecondsToUTCTime . parse) msde)

    liftIO $ forM_ sourceFiles $ \(fn, src) -> do
        let path = chdir </> fn
        BS.writeFile path $ UTF8.fromString src
        setModificationTime path modtime

    action dir
  where
    withDir (Just dir) = \f -> f dir
    withDir Nothing    = withSystemTempDirectoryEnv "cabal-helper-source"


sourceFiles :: [(FilePath, String)]
sourceFiles =
  [ ("Runtime/Main.hs",     $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "src/CabalHelper/Runtime/Main.hs")))
  , ("Runtime/Licenses.hs", $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "src/CabalHelper/Runtime/Licenses.hs")))
  , ("Shared/Common.hs",    $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "src/CabalHelper/Shared/Common.hs")))
  , ("Shared/Sandbox.hs",   $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "src/CabalHelper/Shared/Sandbox.hs")))
  , ("Shared/InterfaceTypes.hs",     $(LitE . StringL <$> runIO (UTF8.toString <$> BS.readFile "src/CabalHelper/Shared/InterfaceTypes.hs")))
  ]
