{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Authorization
    ( authorizeAdmin
    , authorizeRepo
    ) where

import Import.NoFoundation

import Cache
import qualified Data.Text as T
import GitHub.Data (Name, toPathPart)
import qualified GitHub.Data as GH
import Model.Collaborator

authorizeAdmin
    :: MonadHandler m => AppSettings -> Maybe UserId -> SqlPersistT m AuthResult
authorizeAdmin _ Nothing = notFound
authorizeAdmin settings (Just userId) = do
    user <- fromMaybeM notFound =<< get userId
    authorizeWhen $ userEmail user `elem` appAdmins settings

authorizeRepo
    :: (MonadCache m, MonadHandler m)
    => AppSettings
    -> Name GH.Owner
    -> Name GH.Repo
    -> Maybe UserId
    -> SqlPersistT m AuthResult
authorizeRepo _ owner name Nothing = do
    Entity _ repo <- getBy404 $ UniqueRepo owner name

    logDebugN
        $ "Authorizing "
        <> toPathPart owner
        <> "/"
        <> toPathPart name
        <> " for anonymous user"

    authorizeWhen $ not $ repoIsPrivate repo

authorizeRepo settings owner name (Just userId) = do
    Entity _ repo <- getBy404 $ UniqueRepo owner name

    logDebugN
        $ "Authorizing "
        <> toPathPart owner
        <> "/"
        <> toPathPart name
        <> " for authenticated user id="
        <> toPathPiece userId

    if repoIsPrivate repo
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
        [toPathPart owner, toPathPart name, toPathPiece userId]

    -- Log this at INFO temporarily since it's important and new
    logPrivateRepoResult user canRead =
        logInfoN
            $ "GitHub user name="
            <> maybe "<none>" toPathPart (userGithubUsername user)
            <> " repo="
            <> toPathPart owner
            <> "/"
            <> toPathPart name
            <> " can_read="
            <> tshow canRead

authorizeWhen :: MonadHandler m => Bool -> m AuthResult
authorizeWhen True = pure Authorized
authorizeWhen False = notFound
