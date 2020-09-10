{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Restyled.Application
    ( runWaiApp
    )
where

import Restyled.Prelude

import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.ForceSSL
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Restyled.Foundation
import Restyled.Handlers.Admin
import Restyled.Handlers.Admin.Machines
import Restyled.Handlers.Admin.Machines.State
import Restyled.Handlers.Admin.Marketplace
import Restyled.Handlers.Admin.Repos
import Restyled.Handlers.Admin.Token
import Restyled.Handlers.Common
import Restyled.Handlers.GitHubStudents
import Restyled.Handlers.Home
import Restyled.Handlers.Marketplace
import Restyled.Handlers.PrivacyPolicy
import Restyled.Handlers.Profile
import Restyled.Handlers.Repos
import Restyled.Handlers.System.Metrics
import Restyled.Handlers.Thanks
import Restyled.Handlers.Webhooks
import Restyled.Settings
import Restyled.Yesod

mkYesodDispatch "App" resourcesApp

runWaiApp :: App -> IO ()
runWaiApp app = do
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
