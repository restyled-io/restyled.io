{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Foundation
    ( module Foundation
    )
where

import Import.NoFoundation

import Api.Error
import Authentication
import Authorization
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Redis (Connection)
import RIO (HasLogFunc(..), LogFunc, lens)
import RIO.DB hiding (runDB)
import RIO.Process
import RIO.Redis
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth
import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.GitHub
import Yesod.Auth.OAuth2.GitLab
import Yesod.Default.Util (addStaticContentExternal)

data App = App
    { appSettings :: AppSettings
    , appStatic :: Static
    , appConnPool :: ConnectionPool
    , appRedisConn :: Connection
    , appHttpManager :: Manager
    , appLogFunc :: LogFunc
    , appProcessContext :: ProcessContext
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
            addStylesheet $ StaticR css_strapless_css
            addStylesheet $ StaticR css_main_css
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

addOAuth2Plugin
    :: (Text -> Text -> AuthPlugin App)
    -> Maybe OAuthKeys
    -> [AuthPlugin App]
    -> [AuthPlugin App]
addOAuth2Plugin mkPlugin = maybe id $ \OAuthKeys {..} ->
    (<> [mkPlugin oauthKeysClientId oauthKeysClientSecret])

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
