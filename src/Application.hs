{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( appMain
    , makeFoundation
    )
where

import Import

import Control.Monad.Logger (liftLoc)
import Database.Redis (checkedConnect)
import Language.Haskell.TH.Syntax (qLocation)
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
import Network.Wai.Middleware.RequestLogger
    ( Destination(Callback, Logger)
    , IPAddrSource(..)
    , OutputFormat(..)
    , destination
    , mkRequestLogger
    , outputFormat
    )
import RIO (logFuncUseColorL, runRIO, view)
import RIO.DB (createConnectionPool)
import RIO.Logger
import RIO.Orphans ()
import RIO.Process
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
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

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- getGlobalManager
    appLogFunc <- terminalLogFunc $ loggerLogLevel $ appLogLevel appSettings
    appProcessContext <- mkDefaultProcessContext
    appStatic <- (if appMutableStatic appSettings then staticDevel else static)
        appStaticDir
    appRedisConn <- checkedConnect $ appRedisConf appSettings
    appConnPool <- runRIO appLogFunc $ createConnectionPool $ appDatabaseConf
        appSettings

    runRIO appLogFunc $ logInfoN $ "STARTUP{ " <> tshow appSettings <> " }"

    pure App {..}

makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    pure $ logWare $ waiMiddleware appPlain

waiMiddleware :: Middleware
waiMiddleware = forceSSL . methodOverridePost . defaultMiddlewaresNoLogging

makeLogWare :: App -> IO Middleware
makeLogWare foundation = do
    useColor <- runRIO foundation $ view logFuncUseColorL
    loggerSet <- newStdoutLoggerSet defaultBufSize
    mkRequestLogger def
        { outputFormat = if appSettings foundation `allowsLevel` LevelDebug
            then Detailed useColor
            else Apache apacheIpSource
        , destination = if appSettings foundation `allowsLevel` LevelInfo
            then Logger loggerSet
            else Callback $ \_ -> pure ()
        }
  where
    apacheIpSource = if appIpFromHeader $ appSettings foundation
        then FromFallback
        else FromSocket

-- brittany-disable-next-binding

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ logFuncLog
            (appLogFunc foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

appMain :: IO ()
appMain = do
    settings <- loadEnvSettings
    foundation <- makeFoundation settings
    app <- makeApplication foundation

    runSettings (warpSettings foundation) app
