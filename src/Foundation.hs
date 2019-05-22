{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Foundation
    ( module Foundation
    )
where

import Import

import Api.Error
import Authentication
import Authorization
import Data.Text (splitOn)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.GitHub
import Yesod.Auth.OAuth2.GitLab
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Static

data App = App
    { appLogFunc :: LogFunc
    , appSettings :: AppSettings
    , appProcessContext :: ProcessContext
    , appConnPool :: ConnectionPool
    , appRedisConn :: Connection
    , appStatic :: Static
    }

instance HasLogFunc App where
    logFuncL = lens appLogFunc $ \x y -> x
        { appLogFunc = y }

instance HasProcessContext App where
    processContextL = lens appProcessContext $ \x y -> x
        { appProcessContext = y }

instance HasSettings App where
    settingsL = lens appSettings $ \x y -> x
        { appSettings = y }

instance HasDB App where
    dbConnectionPoolL = lens appConnPool $ \x y -> x
        { appConnPool = y }

instance HasRedis App where
    redisConnectionL = lens appRedisConn $ \x y -> x
        { appRedisConn = y }

loadApp :: AppSettings -> IO App
loadApp settings = do
    logFunc <- terminalLogFunc $ appLogLevel settings
    runRIO logFunc $ logInfoN $ pack $ displayAppSettings settings

    App logFunc settings
        <$> mkDefaultProcessContext
        <*> runRIO logFunc (createConnectionPool $ appDatabaseConf settings)
        <*> checkedConnect (appRedisConf settings)
        <*> makeStatic (appStaticDir settings)
  where
    makeStatic
        | appMutableStatic settings = staticDevel
        | otherwise = static

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
        mUser <- maybeAuth

        pc <- widgetToPageContent $ do
            addStylesheet $ staticR "css/strapless.css"
            addStylesheet $ staticR "css/main.css"
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR LoginR

    isAuthorized (AuthR _) _ = pure Authorized
    isAuthorized (StaticR _) _ = pure Authorized
    isAuthorized FaviconR _ = pure Authorized
    isAuthorized WebhooksR _ = pure Authorized
    isAuthorized PrivacyPolicyR _ = pure Authorized
    isAuthorized HomeR _ = pure Authorized
    isAuthorized RevisionR _ = pure Authorized
    isAuthorized RobotsR _ = pure Authorized
    isAuthorized ThanksGitHubR _ = pure Authorized
    isAuthorized ThanksGitHubSetupR _ = pure Authorized

    isAuthorized ProfileR _ =
        maybe AuthenticationRequired (const Authorized) <$> maybeAuthId

    isAuthorized AdminR _ = do
        settings <- getsYesod appSettings
        runDB $ authorizeAdmin settings =<< maybeAuthId

    isAuthorized (AdminP _) _ = do
        settings <- getsYesod appSettings
        runDB $ authorizeAdmin settings =<< maybeAuthId

    isAuthorized (RepoP owner repo _) _ = do
        settings <- getsYesod appSettings
        runDB $ authorizeRepo settings owner repo =<< maybeAuthId

    addStaticContent ext mime content = do
        staticDir <- getsYesod $ appStaticDir . appSettings

        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    messageLoggerSource app _logger = logFuncLog $ appLogFunc app

    defaultMessageWidget title body = $(widgetFile "default-message-widget")

    errorHandler NotFound = do
        mUserId <- maybeAuthId

        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Not Found"
                $(widgetFile "not-found")
            provideRep $ sendApiError $ ApiErrorNotFound $ isJust mUserId

    errorHandler x = defaultErrorHandler x

-- | Just like default-layout, but admin-specific nav and CSS
adminLayout :: Widget -> Handler Html
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    pc <- widgetToPageContent $ do
        addStylesheet $ staticR "css/strapless.css"
        addStylesheet $ staticR "css/main.css"
        addStylesheet $ staticR "css/admin.css"
        addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
        addScriptRemote "https://underscorejs.org/underscore-min.js"
        $(widgetFile "admin-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- | Route to a Static file at a given path
--
-- This removes the type-safety of the TH-defined functions, but makes we don't
-- need compile-time knowledge of our static-directory, and there is value with
-- keeping all possible settings entirely runtime-defined.
--
staticR :: FilePath -> Route App
staticR path = StaticR $ StaticRoute (splitOn "/" $ pack path) []

instance YesodAuth App where
    type AuthId App = UserId

    authenticate = liftHandler . runDB . authenticateUser

    loginDest _ = HomeR
    logoutDest _ = HomeR

    loginHandler = do
        plugins <- getsYesod authPlugins

        let formatPluginName = \case
                "github" -> "GitHub"
                "gitlab" -> "GitLab"
                name -> name

        authLayout $ do
            setTitle "Log In"
            $(widgetFile "login")

    authPlugins App{..} = addAuthBackDoor appSettings
        . addOAuth2Plugin oauth2GitLab (appGitLabOAuthKeys appSettings)
        . addOAuth2Plugin oauth2GitHub (appGitHubOAuthKeys appSettings)
        $ []

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
