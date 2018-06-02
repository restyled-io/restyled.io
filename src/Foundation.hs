{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Foundation where

import Import.NoFoundation

import Data.Aeson
import qualified Data.CaseInsensitive as CI
import qualified Data.Text.Encoding as TE
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Redis (Connection)
import GitHub.Instances (OwnerName, RepoName)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth
import Yesod.Auth.Dummy
import Yesod.Auth.Message (AuthMessage(..))
import Yesod.Auth.OAuth2
import Yesod.Auth.OAuth2.Github
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

-- | Just for reading email out of credsExtra
newtype GitHubUser = GitHubUser { ghuEmail :: Text }

instance FromJSON GitHubUser where
    parseJSON = withObject "GitHubUser" $ \o -> GitHubUser
        <$> o .: "email"

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

    yesodMiddleware = defaultYesodMiddleware

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR css_strapless_css
            addStylesheet $ StaticR css_main_css
            $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    authRoute _ = Just $ AuthR $ oauth2Url "github"

    isAuthorized AdminR _ = authorizeAdmins
    isAuthorized (AdminP _) _ = authorizeAdmins
    isAuthorized _ _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal
        minifym
        genFileName
        appStaticDir
        (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged.
    shouldLogIO app _source level = return
        $ appSettings app `allowsLevel` level

    -- Provide proper Bootstrap styling for default displays, like
    -- error pages
    defaultMessageWidget title body = $(widgetFile "default-message-widget")

-- | Just like default-layout, but admin-specific nav and CSS
adminLayout :: Widget -> Handler Html
adminLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    pc <- widgetToPageContent $ do
        addStylesheet $ StaticR css_strapless_css
        addStylesheet $ StaticR css_main_css
        addStylesheet $ StaticR css_admin_css
        $(widgetFile "admin-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

authorizeAdmins :: Handler AuthResult
authorizeAdmins = do
    admins <- appAdmins <$> getsYesod appSettings
    authorizeAdmin <$> maybeAuth <*> pure admins

authorizeAdmin :: Maybe (Entity User) -> [Text] -> AuthResult
authorizeAdmin Nothing _ = AuthenticationRequired
authorizeAdmin (Just (Entity _ u)) admins
    | userEmail u `elem` admins = Authorized
    | otherwise = Unauthorized "Unauthorized"

instance YesodAuth App where
    type AuthId App = UserId

    authenticate creds@Creds{..} = liftHandler $ runDB $ do
        logDebugN $ "Running authenticate: " <> tshow creds
        muser <- getBy (UniqueUser credsPlugin credsIdent)
        logDebugN $ "Existing user: " <> tshow muser
        let eemail = ghuEmail <$> getUserResponseJSON creds
        logDebugN $ "GitHub email: " <> tshow eemail

        case (entityKey <$> muser, eemail) of
            -- Probably testing via auth/dummy, just authenticate
            (Just uid, Left _) -> pure $ Authenticated uid

            -- New user, create an account
            (Nothing, Right email) -> Authenticated <$> insert User
                { userEmail = email
                , userCredsIdent = credsIdent
                , userCredsPlugin = credsPlugin
                }

            -- Existing user, synchronize email
            (Just uid, Right email) -> do
                update uid [UserEmail =. email]
                pure $ Authenticated uid

            -- Unexpected, no email in GH response
            (Nothing, Left err) -> do
                logWarnN $ "Error parsing user response: " <> pack err
                pure $ UserError $ IdentifierNotFound "email"

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

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlBackend
    runDB action = do
        master <- getYesod
        runSqlPool action $ appConnPool master
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner appConnPool

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
