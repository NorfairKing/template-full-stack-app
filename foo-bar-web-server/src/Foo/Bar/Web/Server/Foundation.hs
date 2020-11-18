{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Foo.Bar.Web.Server.Foundation where

import Data.Text (Text)
import Foo.Bar.Web.Server.Widget
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Types as Http
import Servant.Client
import Text.Hamlet
import Yesod
import Yesod.EmbeddedStatic

data App
  = App
      { appLogLevel :: !LogLevel,
        appStatic :: !EmbeddedStatic,
        appAPIUrl :: !BaseUrl,
        appHttpManager :: !Http.Manager,
        appGoogleAnalyticsTracking :: !(Maybe Text),
        appGoogleSearchConsoleVerification :: !(Maybe Text)
      }

mkYesodData "App" $(parseRoutesFile "routes.txt")

instance Yesod App where
  shouldLogIO app _ ll = pure $ ll >= appLogLevel app
  defaultLayout widget = do
    app <- getYesod
    pageContent <- widgetToPageContent $(widgetFile "default-body")
    withUrlRenderer $(hamletFile "templates/default-page.hamlet")

runClientSafe :: ClientM a -> Handler (Either ClientError a)
runClientSafe func = do
  man <- getsYesod appHttpManager
  burl <- getsYesod appAPIUrl
  let cenv = mkClientEnv man burl
  liftIO $ runClientM func cenv

runClientOrError :: ClientM a -> Handler a
runClientOrError func = do
  errOrRes <- runClientSafe func
  case errOrRes of
    Left err -> handleStandardServantErrs err $ \resp -> error $ show resp -- TODO deal with error in an application-specific way
    Right r -> pure r

handleStandardServantErrs :: ClientError -> (Response -> Handler a) -> Handler a
handleStandardServantErrs err func =
  case err of
    FailureResponse _ resp -> func resp
    ConnectionError e -> error $ unwords ["The api seems to be down:", show e]
    e -> error $ unwords ["Error while calling API:", show e]
