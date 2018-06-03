{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    ) where

import Import

import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql
    (createPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool)
import Database.Redis (checkedConnect)
import Language.Haskell.TH.Syntax (qLocation)
import LoadEnv (loadEnv)
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
import Network.Wai.Middleware.MethodOverridePost
import Network.Wai.Middleware.RequestLogger
    ( Destination(Callback, Logger)
    , IPAddrSource(..)
    , OutputFormat(..)
    , destination
    , mkRequestLogger
    , outputFormat
    )
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Yesod.Auth
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2 (develMainHelper, getDevSettings, makeYesodLogger)

import Handler.Common
import Handler.Home
import Handler.Repos
import Handler.Signups
import Handler.Webhooks

import Handler.Admin
import Handler.Admin.Jobs
import Handler.Admin.Repos
import Handler.Admin.Signups

mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <- (if appMutableStatic appSettings
        then staticDevel
        else static) appStaticDir

    -- Persistent Redis connection
    appRedisConn <- checkedConnect $ appRedisConf appSettings

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App{..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    -- Perform database migration using our application's logging settings.
    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    -- Output settings at startup
    runLoggingT (logInfoN $ "settings " <> tshow appSettings) logFunc

    return $ mkFoundation pool

makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ waiMiddleware appPlain

waiMiddleware :: Middleware
waiMiddleware = methodOverridePost . defaultMiddlewaresNoLogging

makeLogWare :: App -> IO Middleware
makeLogWare foundation = do
    isTTY <- queryTerminal stdOutput
    mkRequestLogger def
        { outputFormat = if appSettings foundation `allowsLevel` LevelDebug
            then Detailed isTTY
            else Apache apacheIpSource
        , destination = if appSettings foundation `allowsLevel` LevelInfo
            then Logger $ loggerSet $ appLogger foundation
            else Callback $ \_ -> return ()
        }
  where
    apacheIpSource = if appIpFromHeader $ appSettings foundation
        then FromFallback
        else FromSocket

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = do
    loadEnv
    loadEnvSettings

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from ENV
    settings <- getAppSettings

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app
