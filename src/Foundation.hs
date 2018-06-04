{-# LANGUAGE DeriveGeneric #-}
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

import Data.Aeson
import Data.Aeson.Casing
import Database.Persist.Sql (ConnectionPool, runSqlPool)
import Database.Redis (Connection)
import GitHub.Data (Id, Name)
import qualified GitHub.Data as GH
import GitHub.Instances (OwnerName, RepoName)
import Model.Collaborator
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

-- | For reading profile out of credsExtra
data GitHubUser = GitHubUser
    { ghuEmail :: Text
    , ghuId :: Id User
    , ghuLogin :: Name User
    }
    deriving (Eq, Show, Generic)

instance FromJSON GitHubUser where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

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
    isAuthorized (OwnerP owner (ReposP (RepoP repo _))) _ = authorizeRepo owner repo
    isAuthorized _ _ = return Authorized

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

-- brittany-disable-next-binding
authorizeRepo :: GH.Name GH.Owner -> GH.Name GH.Repo -> Handler AuthResult
authorizeRepo owner name = do
    mUserId <- maybeAuthId

    logDebugN "Authorizing repo"
    logDebugN $ "UserId: " <> tshow mUserId

    (mUser, Entity _ repo) <- runDB $ (,)
        <$> (join <$> traverse get mUserId)
        <*> getBy404 (UniqueRepo owner name)

    if repoIsPrivate repo
        then authorizePrivateRepo mUser repo
        else logDebugN "Public repo authorized" $> Authorized

-- TODO: cache
authorizePrivateRepo :: Maybe User -> Repo -> Handler AuthResult
authorizePrivateRepo mUser repo = do
    user <- fromMaybeM notFound mUser
    (appId, appKey) <-
        getsYesod $ (appGitHubAppId &&& appGitHubAppKey) . appSettings
    canRead <- collaboratorCanRead appId appKey repo user

    logDebugN $ "User: " <> tshow user
    logDebugN $ "Can-Read: " <> tshow canRead

    if canRead then pure Authorized else notFound

instance YesodAuth App where
    type AuthId App = UserId

    authenticate creds@Creds{..} = liftHandler $ runDB $ do
        logDebugN $ "Running authenticate: " <> tshow creds
        muser <- getBy (UniqueUser credsPlugin credsIdent)
        logDebugN $ "Existing user: " <> tshow muser
        let euser = getUserResponseJSON creds
        logDebugN $ "GitHub user: " <> tshow euser

        case (entityKey <$> muser, euser) of
            -- Probably testing via auth/dummy, just authenticate
            (Just uid, Left _) -> pure $ Authenticated uid

            -- New user, create an account
            (Nothing, Right GitHubUser{..}) -> Authenticated <$> insert User
                { userEmail = ghuEmail
                , userGithubUserId = Just ghuId
                , userGithubUsername = Just ghuLogin
                , userCredsIdent = credsIdent
                , userCredsPlugin = credsPlugin
                }

            -- Existing user, synchronize email
            (Just uid, Right GitHubUser{..}) -> do
                update uid
                    [ UserEmail =. ghuEmail
                    , UserGithubUserId =. Just ghuId
                    , UserGithubUsername =. Just ghuLogin
                    ]
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

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
