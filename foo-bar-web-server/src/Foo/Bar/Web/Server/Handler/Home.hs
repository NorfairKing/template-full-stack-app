{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Foo.Bar.Web.Server.Handler.Home where

import Foo.Bar.Web.Server.Handler.Import

getHomeR :: Handler Html
getHomeR = do
  let token = undefined
  greeting <- runClientOrError $ getGreet fooBarClient token
  defaultLayout $(widgetFile "home")
