module Restyled.Authorization
    ( authorizeAdmin
    , authorizeRepo

    -- * Exported for re-use in tests
    , authRepoCacheKey
    ) where

import Restyled.Prelude

import Restyled.Cache
import Restyled.Models
import Restyled.Settings
import Restyled.Yesod

authorizeAdmin
    :: MonadHandler m => AppSettings -> Maybe UserId -> SqlPersistT m AuthResult
authorizeAdmin _ Nothing = notFound
authorizeAdmin settings (Just userId) = do
    user <- fromMaybeM notFound $ get userId
    authorizeWhen $ userIsAdmin settings user

authorizeRepo
    :: (HasCallStack, MonadCache m, MonadHandler m)
    => AppSettings
    -> OwnerName
    -> RepoName
    -> Maybe UserId
    -> SqlPersistT m AuthResult
authorizeRepo settings owner name mUserId = do
    -- We only support checking collaborator access for GitHub right now. This
    -- will naturally return 404 for other cases for now.
    Entity _ repo <- getBy404 $ UniqueRepo GitHubSVCS owner name

    if repoIsPrivate repo
        then do
            mUser <- join <$> traverse get mUserId
            maybe notFound (authorizePrivateRepo settings repo) mUser
        else pure Authorized

-- | Authorize if the @'User'@ is a Collaborator according to GitHub
authorizePrivateRepo
    :: (HasCallStack, MonadCache m, MonadHandler m)
    => AppSettings
    -> Repo
    -> User
    -> SqlPersistT m AuthResult
authorizePrivateRepo settings@AppSettings {..} repo@Repo {..} user@User {..} =
    do
        result <- runExceptT $ do
            username <- userGithubUsername ?? "User has no GitHub Username"
            caching (authRepoCacheKey repo username) $ withExceptT show $ do
                auth <- ExceptT $ liftIO $ githubAuthInstallation
                    appGitHubAppId
                    appGitHubAppKey
                    repoInstallationId

                permissions <- ExceptT $ liftIO $ collaboratorPermissions
                    auth
                    repoOwner
                    repoName
                    username

                pure $ collaboratorCanRead permissions

        let isAdmin = userIsAdmin settings user
            (granted, reason, mErr) = resolveAuth isAdmin result

        logInfoN $ mconcat
            [ "AUTHORIZE"
            , " user=" <> maybe "<unknown>" toPathPiece userGithubUsername
            , " repo=" <> repoPath repoOwner repoName
            , " granted=" <> tshow granted
            , " reason=" <> tshow reason
            , " error=" <> maybe "" tshow mErr
            ]

        authorizeWhen granted

authRepoCacheKey :: Repo -> GitHubUserName -> [Text]
authRepoCacheKey Repo {..} username =
    [ "auth"
    , "repo"
    , toPathPiece repoSvcs
    , toPathPiece repoOwner
    , toPathPiece repoName
    , toPathPiece username
    ]

-- brittany-disable-next-binding
-- Context-sensitive alignment helps visually verify the cases

resolveAuth :: Bool -> Either String Bool -> (Bool, Text, Maybe String)
resolveAuth True  (Right True)  = (True,  "admin+collaborator", Nothing)
resolveAuth True  (Right False) = (True,  "admin",              Nothing)
resolveAuth True  (Left  err)   = (True,  "admin",              Just err)
resolveAuth False (Right True)  = (True,  "collaborator",       Nothing)
resolveAuth False (Right False) = (False, "non-collaborator",   Nothing)
resolveAuth False (Left  err)   = (False, "error",              Just err)

authorizeWhen :: MonadHandler m => Bool -> m AuthResult
authorizeWhen True = pure Authorized
authorizeWhen False = notFound
