{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( appMain
    )
where

import Import

import Foundation
import Handler.Admin
import Handler.Admin.Jobs
import Handler.Admin.Machines
import Handler.Admin.Marketplace
import Handler.Admin.Repos
import Handler.Common
import Handler.Home
import Handler.PrivacyPolicy
import Handler.Profile
import Handler.Repos
import Handler.Thanks
import Handler.Webhooks
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.ForceSSL
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Yesod
import Yesod.Auth

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
    setLineBuffering

    loadEnv
    app <- loadApp =<< loadEnvSettings

    waiApp <- waiMiddleware app <$> toWaiAppPlain app
    runSettings (warpSettings app) waiApp

waiMiddleware :: App -> Middleware
waiMiddleware app =
    forceSSL . methodOverridePost . requestLogger . defaultMiddlewaresNoLogging
  where
    requestLogger
        | appDetailedRequestLogger (appSettings app) = logStdoutDev
        | otherwise = logStdout

warpSettings :: App -> Settings
warpSettings app =
    setHost host . setPort port . setOnException onEx $ defaultSettings
  where
    port = appPort $ appSettings app
    host = appHost $ appSettings app
    onEx _req ex =
        when (defaultShouldDisplayException ex)
            $ runRIO app
            $ logErrorN
            $ "Warp exception: "
            <> tshow ex
