{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( appMain
    , makeFoundation
    , makeLogWare
    )
where

import Import

import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql (createPostgresqlPool, pgConnStr, pgPoolSize)
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
import System.Log.FastLogger (defaultBufSize, newStdoutLoggerSet, toLogStr)
import System.Posix.IO (stdOutput)
import System.Posix.Terminal (queryTerminal)
import Yesod.Auth
import Yesod.Core.Types (loggerSet)
import Yesod.Default.Config2 (makeYesodLogger)

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
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <- (if appMutableStatic appSettings then staticDevel else static)
        appStaticDir
    appRedisConn <- checkedConnect $ appRedisConf appSettings

    let mkFoundation appConnPool = App {..}
        tempFoundation =
            mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    runLoggingT (logInfoN $ "STARTUP{ " <> tshow appSettings <> " }") logFunc

    pure $ mkFoundation pool

makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    appPlain <- toWaiAppPlain foundation
    pure $ logWare $ waiMiddleware appPlain

waiMiddleware :: Middleware
waiMiddleware = forceSSL . methodOverridePost . defaultMiddlewaresNoLogging

makeLogWare :: App -> IO Middleware
makeLogWare foundation = do
    isTTY <- queryTerminal stdOutput
    mkRequestLogger def
        { outputFormat = if appSettings foundation `allowsLevel` LevelDebug
            then Detailed isTTY
            else Apache apacheIpSource
        , destination = if appSettings foundation `allowsLevel` LevelInfo
            then Logger $ loggerSet $ appLogger foundation
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
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
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
