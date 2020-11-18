{-# LANGUAGE RecordWildCards #-}

module Foo.Bar.Web.Server where

import Control.Monad
import Foo.Bar.Web.Server.Application ()
import Foo.Bar.Web.Server.Constants
import Foo.Bar.Web.Server.Foundation
import Foo.Bar.Web.Server.OptParse
import Foo.Bar.Web.Server.Static
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.TLS as Http
import Text.Show.Pretty
import Yesod

fooBarWebServer :: IO ()
fooBarWebServer = do
  sets <- getSettings
  when development $ pPrint sets
  runFooBarWebServer sets

runFooBarWebServer :: Settings -> IO ()
runFooBarWebServer Settings {..} = do
  man <- liftIO $ Http.newManager Http.tlsManagerSettings
  let app =
        App
          { appLogLevel = settingLogLevel,
            appStatic = fooBarWebServerStatic,
            appAPIUrl = settingApiUrl,
            appHttpManager = man,
            appGoogleAnalyticsTracking = settingGoogleAnalyticsTracking,
            appGoogleSearchConsoleVerification = settingGoogleSearchConsoleVerification
          }
  Yesod.warp settingPort app
