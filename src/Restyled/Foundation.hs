{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}
{-# OPTIONS_GHC -Wno-missing-local-signatures #-}

module Restyled.Foundation
    ( module Restyled.Foundation
    ) where

import Restyled.Prelude

import qualified Amazonka as AWS (Env)
import Data.Text (splitOn)
import Logging.Logger (getLoggerLoggerSet)
import Restyled.ApiError
import Restyled.ApiToken
import Restyled.Authentication
import Restyled.Authorization
import Restyled.Models
import Restyled.Queues
import Restyled.ServerUnavailable
import Restyled.Settings
import Restyled.SqlError
import Restyled.Tracing
import Restyled.Yesod
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import qualified Yesod.Core.Types as Y
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Persist (YesodPersist)
import qualified Yesod.Persist as YP
import Yesod.Static

data App = App
    { appSettings :: AppSettings
    , appLogger :: Logger
    , appConnPool :: ConnectionPool
    , appRedisConn :: Connection
    , appAWSEnv :: AWS.Env
    , appTracingApp :: TracingApp
    , appStatic :: Static
    }

instance HasSettings App where
    settingsL = lens appSettings $ \x y -> x { appSettings = y }

instance HasLogger App where
    loggerL = lens appLogger $ \x y -> x { appLogger = y }

instance HasQueues App where
    queuesL = settingsL . queuesL

instance HasSqlPool App where
    sqlPoolL = lens appConnPool $ \x y -> x { appConnPool = y }

instance HasRedis App where
    redisConnectionL = lens appRedisConn $ \x y -> x { appRedisConn = y }

instance HasAWS App where
    awsEnvL = lens appAWSEnv $ \x y -> x { appAWSEnv = y }

instance HasTracingApp App where
    tracingAppL = lens appTracingApp $ \x y -> x { appTracingApp = y }

loadApp :: IO App
loadApp = do
    settings@AppSettings {..} <- loadSettings

    let createPool = createConnectionPool appDatabaseConf appStatementTimeout
        makeStatic
            | appMutableStatic = staticDevel
            | otherwise = static

    logger <- newLogger $ setLogSettingsLevel appLogLevel defaultLogSettings

    App settings logger
        <$> runLoggerLoggingT logger createPool
        <*> checkedConnect appRedisConf
        <*> discoverAWS appAwsTrace
        <*> newTracingApp appTracingConfig
        <*> makeStatic appStaticDir

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- brittany-disable-next-binding

instance Yesod App where
    approot = ApprootMaster $ appRoot . view settingsL

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
    isAuthorized (OffersP _) _ = pure Authorized
    isAuthorized ThanksGitHubR _ = pure Authorized
    isAuthorized ThanksGitHubSetupR _ = pure Authorized
    isAuthorized (GitHubStudentsP _) _ = pure Authorized

    isAuthorized ProfileR _ = traceAppSegment "Authorize" $ do
        maybe AuthenticationRequired (const Authorized) <$> maybeAuthId

    isAuthorized AdminR _ = traceAppSegment "Authorize" $ do
        settings <- getsYesod $ view settingsL
        authorizeAdmin settings =<< maybeAuthId

    isAuthorized (AdminP _) _ = traceAppSegment "Authorize" $ do
        settings <- getsYesod $ view settingsL
        authorizeAdmin settings =<< maybeAuthId

    -- Jobs API is Admin-only
    isAuthorized (JobsP _) _ = traceAppSegment "Authorize" $ do
        settings <- getsYesod $ view settingsL
        authorizeAdmin settings =<< maybeAuthId

    -- Writing RepoR is Admin-only
    isAuthorized (RepoP _ _ RepoR) True = traceAppSegment "Authorize" $ do
        settings <- getsYesod $ view settingsL
        authorizeAdmin settings =<< maybeAuthId

    isAuthorized (RepoP owner repo _) _ = traceAppSegment "Authorize" $ do
        settings <- getsYesod $ view settingsL
        authorizeRepo settings owner repo =<< maybeAuthId

    addStaticContent ext mime content = do
        staticDir <- getsYesod $ appStaticDir . view settingsL

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

    makeLogger App {..} = do
        logger <- defaultMakeLogger
        pure $ logger { Y.loggerSet = getLoggerLoggerSet appLogger }

    messageLoggerSource app _logger loc source level msg =
        runLoggerLoggingT app $ monadLoggerLog loc source level msg

    yesodMiddleware handler = traceAppSegment "Handler" $ do
        handleSqlErrorState sqlStateQueryCanceled
            (\_ -> serverUnavailable "Database operation took too long")
            (defaultYesodMiddleware handler)

    defaultMessageWidget title body = $(widgetFile "default-message-widget")

    errorHandler NotFound = do
        mUserId <- maybeAuthId

        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Not Found"
                $(widgetFile "not-found")
            provideRep $ sendApiError $ ApiErrorNotFound $ isJust mUserId

    errorHandler x | Just message <- serverUnavailableMessage x = do
        selectRep $ do
            provideRep $ defaultLayout $ do
                setTitle "Unavailable"
                $(widgetFile "unavailable")
            provideRep $ sendApiError $ ApiErrorUnavailable message

    errorHandler x = defaultErrorHandler x


-- | Just like default-layout, but admin-specific nav and CSS
adminLayout :: Widget -> Handler Html
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    pc <- widgetToPageContent $ do
        addScript $ staticR "js/admin.js"
        addStylesheet $ staticR "css/strapless.css"
        addStylesheet $ staticR "css/main.css"
        addStylesheet $ staticR "css/admin.css"
        addScriptRemote "https://underscorejs.org/underscore-min.js"
        addScriptRemote "https://code.jquery.com/jquery-3.3.1.min.js"
        $(widgetFile "admin-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

-- | Non-layout for delivering HTML fragments to load into the DOM via JS
fragmentLayout :: Widget -> Handler Html
fragmentLayout = withUrlRenderer . pageBody <=< widgetToPageContent

-- | Route to a Static file at a given path
--
-- This removes the type-safety of the TH-defined functions, but makes we don't
-- need compile-time knowledge of our static-directory, and there is value with
-- keeping all possible settings entirely runtime-defined.
--
staticR :: FilePath -> Route App
staticR path = StaticR $ StaticRoute (splitOn "/" $ pack path) []

-- | Only needed for YesodAuth to work, we use @'RIO.DB.runDB'@ directly
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB = runDB

instance YesodAuthPersist App

-- brittany-disable-next-binding

instance YesodAuth App where
    type AuthId App = UserId

    authenticate = liftHandler
        . traceAppSegment "Authenticate"
        . runDB
        . authenticateUser

    loginDest _ = HomeR
    logoutDest _ = HomeR

    loginHandler = do
        plugins <- getsYesod authPlugins

        let
            -- Nothing results in hiding that plugin on the Login page
            formatPluginName :: Text -> Maybe Text
            formatPluginName = \case
                "github" -> Just "GitHub"
                "gitlab" -> Just "GitLab"
                _ -> Nothing

        authLayout $ do
            setTitle "Log In"
            $(widgetFile "login")

    authPlugins app = addAuthBackDoor (app ^. settingsL)
        . addOAuth2Plugin oauth2GitLab (appGitLabOAuthKeys $ app ^. settingsL)
        . addOAuth2Plugin oauth2GitHub (appGitHubOAuthKeys $ app ^. settingsL)
        . addOAuth2Plugin oauth2GitHubStudents (appGitHubStudentsOAuthKeys $ app ^. settingsL)
        $ []

    maybeAuthId = runMaybeT $ asum
        [ MaybeT defaultMaybeAuthId
        , MaybeT $ liftHandler $ runDB getUserIdByApiToken
        ]

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage
