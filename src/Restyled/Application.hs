{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Application
    ( appMain
    )
where

import Restyled.Prelude

import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.ForceSSL
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Restyled.Backend.Foundation
import Restyled.Foundation
import Restyled.Handlers.Admin
import Restyled.Handlers.Admin.Jobs
import Restyled.Handlers.Admin.Machines
import Restyled.Handlers.Admin.Marketplace
import Restyled.Handlers.Admin.Repos
import Restyled.Handlers.Common
import Restyled.Handlers.Home
import Restyled.Handlers.PrivacyPolicy
import Restyled.Handlers.Profile
import Restyled.Handlers.Repos
import Restyled.Handlers.Thanks
import Restyled.Handlers.Webhooks
import Restyled.Settings
import Restyled.Yesod

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
    setLineBuffering

    loadEnv
    app <- loadApp =<< loadBackend =<< loadSettings

    waiApp <- waiMiddleware app <$> toWaiAppPlain app
    runSettings (warpSettings app) waiApp

waiMiddleware :: App -> Middleware
waiMiddleware app =
    forceSSL . methodOverridePost . requestLogger . defaultMiddlewaresNoLogging
  where
    requestLogger
        | appDetailedRequestLogger (app ^. settingsL) = logStdoutDev
        | otherwise = logStdout

warpSettings :: App -> Settings
warpSettings app =
    setHost host . setPort port . setOnException onEx $ defaultSettings
  where
    port = appPort $ app ^. settingsL
    host = appHost $ app ^. settingsL
    onEx _req ex =
        when (defaultShouldDisplayException ex)
            $ runRIO app
            $ logErrorN
            $ "Warp exception: "
            <> tshow ex
