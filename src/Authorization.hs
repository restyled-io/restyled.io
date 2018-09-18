{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Authorization
    ( authorizeAdmin
    , authorizeRepo
    ) where

import Import.NoFoundation

import Cache
import Control.Monad.Except
import SVCS.GitHub.AccessToken
import SVCS.GitHub.Collaborator

authorizeAdmin
    :: MonadHandler m => AppSettings -> Maybe UserId -> SqlPersistT m AuthResult
authorizeAdmin _ Nothing = notFound
authorizeAdmin settings (Just userId) = do
    user <- fromMaybeM notFound =<< get userId
    authorizeWhen $ userEmail user `elem` appAdmins settings

authorizeRepo
    :: (MonadCache m, MonadHandler m)
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
    :: (MonadCache m, MonadHandler m)
    => AppSettings
    -> Repo
    -> User
    -> SqlPersistT m AuthResult
authorizePrivateRepo AppSettings {..} Repo {..} User {..} = do
    result <- runExceptT $ do
        username <- maybeToExceptT "User has no GitHub Username"
            $ liftMaybe userGithubUsername
        caching (cacheKey username) $ do
            token <- ExceptT $ liftIO $ githubInstallationToken
                appGitHubAppId
                appGitHubAppKey
                repoInstallationId
            githubCollaboratorCanRead token repoOwner repoName username

    logInfoN
        $ "Authorization result for "
        <> repoPath repoOwner repoName
        <> " for "
        <> maybe "<unknown>" toPathPiece userGithubUsername
        <> ": "
        <> tshow result

    either (const notFound) authorizeWhen result
  where
    cacheKey username =
        [ "auth"
        , "repo"
        , toPathPiece repoSvcs
        , toPathPiece repoOwner
        , toPathPiece repoName
        , toPathPiece username
        ]

authorizeWhen :: MonadHandler m => Bool -> m AuthResult
authorizeWhen True = pure Authorized
authorizeWhen False = notFound
