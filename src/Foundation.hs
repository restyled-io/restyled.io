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
import Cache
import Data.Aeson (decode, encode)
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Redis (Connection)
import qualified Database.Redis as Redis
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth
import Yesod.Auth.Dummy
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.GitHub
import Yesod.Auth.OAuth2.GitLab
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
