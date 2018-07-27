{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Foundation
    ( module Foundation
    )
where

import Import.NoFoundation

import Authentication
import Authorization
import Cache
import Data.Aeson (decode, encode)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Redis (Connection)
import qualified Database.Redis as Redis
import GitHub.Instances (OwnerName, RepoName)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth
import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.Github
import Yesod.Core.Types (Logger)
import Yesod.Default.Util (addStaticContentExternal)

data App = App
    { appSettings :: AppSettings
    , appStatic :: Static
    , appConnPool :: ConnectionPool
    , appRedisConn :: Connection
    , appHttpManager :: Manager
    , appLogger :: Logger
    }

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . appSettings

    makeSessionBackend _ = Just
        -- 2 week session timeout
        <$> envClientSessionBackend (60 * 24 * 14) "SESSION_KEY"

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        mUserId <- maybeAuthId
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_strapless_css
            addStylesheet $ StaticR css_main_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR $ oauth2Url "github"

    isAuthorized (AuthR _) _ = pure Authorized
    isAuthorized (StaticR _) _ = pure Authorized
    isAuthorized FaviconR _ = pure Authorized
    isAuthorized WebhooksR _ = pure Authorized
    isAuthorized PrivacyPolicyR _ = pure Authorized
    isAuthorized HomeR _ = pure Authorized
    isAuthorized RevisionR _ = pure Authorized
    isAuthorized RobotsR _ = pure Authorized

    isAuthorized AdminR _ = do
        settings <- getsYesod appSettings
        runDB $ authorizeAdmin settings =<< maybeAuthId

    isAuthorized (AdminP _) _ = do
        settings <- getsYesod appSettings
        runDB $ authorizeAdmin settings =<< maybeAuthId

    isAuthorized (RepoP owner repo _) _ = do
        settings <- getsYesod appSettings
        runDB $ authorizeRepo settings owner repo =<< maybeAuthId

    addStaticContent = addStaticContentExternal
        minifym
        genFileName
        appStaticDir
        (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    shouldLogIO app _source level = return
        $ appSettings app `allowsLevel` level

    defaultMessageWidget title body = $(widgetFile "default-message-widget")

    errorHandler NotFound = do
        mUserId <- maybeAuthId
        html <- defaultLayout $ do
            setTitle "Not Found"
            $(widgetFile "not-found")

        pure $ TypedContent typeHtml $ toContent html

    errorHandler x = defaultErrorHandler x

-- | Just like default-layout, but admin-specific nav and CSS
adminLayout :: Widget -> Handler Html
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    mUserId <- maybeAuthId
    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_strapless_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_admin_css
        addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
        addScriptRemote "https://underscorejs.org/underscore-min.js"
        $(widgetFile "admin-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance YesodAuth App where
    type AuthId App = UserId

    authenticate = liftHandler . runDB . authenticateUser

    loginDest _ = HomeR
    logoutDest _ = HomeR

    authPlugins App{..} = addAuthBackDoor appSettings
        [ oauth2Github
            (oauthKeysClientId $ appGitHubOAuthKeys appSettings)
            (oauthKeysClientSecret $ appGitHubOAuthKeys appSettings)
        ]

addAuthBackDoor :: AppSettings -> [AuthPlugin App] -> [AuthPlugin App]
addAuthBackDoor AppSettings {..} =
    if appAllowDummyAuth then (authDummy :) else id

instance YesodAuthPersist App

instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
    getHttpManager = appHttpManager

instance MonadCache (HandlerFor App) where
    getCache (CacheKey key) = do
        conn <- getsYesod appRedisConn
        eVal <- liftIO $ Redis.runRedis conn $ Redis.get $ encodeUtf8 key
        pure $ decode . fromStrict =<< join (hush eVal)

    setCache (CacheKey key) obj = do
        conn <- getsYesod appRedisConn
        void $ liftIO $ Redis.runRedis conn $ do
            void $ Redis.set (encodeUtf8 key) $ toStrict $ encode obj
            void $ Redis.expire (encodeUtf8 key) 300
