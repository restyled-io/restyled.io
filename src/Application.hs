{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( appMain
    , loadApp
    )
where

import Import

import Database.Redis (checkedConnect)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
    ( Settings
    , defaultSettings
    , defaultShouldDisplayException
    , runSettings
    , setHost
    , setOnException
    , setPort
    )
import Network.Wai.Middleware.ForceSSL
import Network.Wai.Middleware.MethodOverridePost
import RIO (runRIO)
import RIO.DB (createConnectionPool)
import RIO.Logger
import RIO.Orphans ()
import RIO.Process
import Settings (requestLogger)
import Yesod.Auth

import Handler.Common
import Handler.Home
import Handler.PrivacyPolicy
import Handler.Profile
import Handler.Repos
import Handler.Thanks
import Handler.Webhooks

import Handler.Admin
import Handler.Admin.Jobs
import Handler.Admin.Machines
import Handler.Admin.Marketplace
import Handler.Admin.Repos

mkYesodDispatch "App" resourcesApp

appMain :: IO ()
appMain = do
    -- Ensure container logs are visible immediately
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    app <- loadApp =<< loadEnvSettings
    runSettings (warpSettings app) . waiMiddleware =<< toWaiAppPlain app

loadApp :: AppSettings -> IO App
loadApp settings = do
    logFunc <- terminalLogFunc $ loggerLogLevel $ appLogLevel settings

    runRIO logFunc $ logInfoN $ "STARTUP{ " <> tshow settings <> " }"

    App settings
        <$> makeStatic appStaticDir
        <*> runRIO logFunc (createConnectionPool $ appDatabaseConf settings)
        <*> checkedConnect (appRedisConf settings)
        <*> getGlobalManager
        <*> pure logFunc
        <*> mkDefaultProcessContext

waiMiddleware :: Middleware
waiMiddleware =
    forceSSL . methodOverridePost . requestLogger . defaultMiddlewaresNoLogging

warpSettings :: App -> Settings
warpSettings app =
    setPort (appPort $ appSettings app)
        . setHost (appHost $ appSettings app)
        . setOnException onWarpException
        $ defaultSettings
  where
    onWarpException _req ex =
        when (defaultShouldDisplayException ex)
            $ runRIO app
            $ logErrorN
            $ "Warp exception: "
            <> tshow ex
