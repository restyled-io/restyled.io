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
    repo <- getBy404 $ UniqueRepo owner name
    authorizeRepo' settings repo mUserId

authorizeRepo'
    :: (MonadCache m, MonadHandler m)
    => AppSettings
    -> Entity Repo
    -> Maybe UserId
    -> SqlPersistT m AuthResult
authorizeRepo' _ (Entity _ Repo {..}) _
    | not repoIsPrivate = pure Authorized
    | repoSvcs /= GitHubSVCS = notFound
authorizeRepo' _ _ Nothing = notFound

-- By this point, we know:
--
-- 1. The repo is private
-- 2. The repo is GitHub
-- 3. The user is authenticated
--
-- So we just go ahead with the GitHub-specific collaborators check.
--
authorizeRepo' settings repo (Just userId) = do
    logDebugN
        $ "Authorizing private GitHub repository"
        <> repoPath owner name
        <> " for authenticated user id="
        <> toPathPiece userId

    User {..} <- get404 userId
    canRead <- caching cacheKey $ runCanRead $ do
        token <- ExceptT $ repoAccessToken settings repo Nothing
        username <- liftEither $ note "No GitHub username" userGithubUsername
        githubCollaboratorCanRead token owner name username

    logInfoN
        $ "Authentication result"
        <> " github_username="
        <> maybe "<none>" toPathPiece userGithubUsername
        <> " repo="
        <> repoPath owner name
        <> " can_read="
        <> tshow canRead

    authorizeWhen canRead
  where
    owner = repoOwner $ entityVal repo
    name = repoName $ entityVal repo
    cacheKey =
        [ "auth"
        , "repo"
        , toPathPiece owner
        , toPathPiece name
        , toPathPiece userId
        ]

authorizeWhen :: MonadHandler m => Bool -> m AuthResult
authorizeWhen True = pure Authorized
authorizeWhen False = notFound

-- | Run the Collaborator check and log-mask errors as @'False'@
runCanRead :: MonadLogger m => ExceptT String m Bool -> m Bool
runCanRead = either ((False <$) . logWarnN . pack) pure <=< runExceptT
