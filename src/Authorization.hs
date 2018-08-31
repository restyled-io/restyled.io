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
import qualified Data.Text as T
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
authorizeRepo _ owner name Nothing = do
    Entity _ repo <- getBy404 $ UniqueRepo owner name

    logDebugN
        $ "Authorizing "
        <> toPathPiece owner
        <> "/"
        <> toPathPiece name
        <> " for anonymous user"

    authorizeWhen $ not $ repoIsPrivate repo

authorizeRepo settings owner name (Just userId) = do
    repo <- getBy404 $ UniqueRepo owner name

    logDebugN
        $ "Authorizing "
        <> toPathPiece owner
        <> "/"
        <> toPathPiece name
        <> " for authenticated user id="
        <> toPathPiece userId

    if repoIsPrivate $ entityVal repo
        then do
            user <- get404 userId
            canRead <- caching $ collaboratorCanRead settings repo user
            logPrivateRepoResult user canRead
            authorizeWhen canRead
        else pure Authorized
  where
    caching = lift . withCache cacheKey
    cacheKey = CacheKey $ "auth.repo." <> T.intercalate
        "."
        [toPathPiece owner, toPathPiece name, toPathPiece userId]

    -- Log this at INFO temporarily since it's important and new
    logPrivateRepoResult user canRead =
        logInfoN
            $ "GitHub user name="
            <> maybe "<none>" toPathPiece (userGithubUsername user)
            <> " repo="
            <> toPathPiece owner
            <> "/"
            <> toPathPiece name
            <> " can_read="
            <> tshow canRead

authorizeWhen :: MonadHandler m => Bool -> m AuthResult
authorizeWhen True = pure Authorized
authorizeWhen False = notFound

collaboratorCanRead
    :: (MonadIO m, MonadLogger m)
    => AppSettings
    -> Entity Repo
    -> User
    -> m Bool
collaboratorCanRead settings e@(Entity _ repo) User {..} = do
    result <- runExceptT $ do
        token <- ExceptT $ repoAccessToken settings e

        case repoSVCS repo of
            GitHubSVCS -> do
                username <- liftEither
                    $ note "No GitHub username" userGithubUsername
                githubCollaboratorCanRead
                    token
                    (repoOwner repo)
                    (repoName repo)
                    username

    either err pure result
  where
    err msg = do
        logErrorN $ "Error authorizing repository:\n" <> pack msg
        pure False
