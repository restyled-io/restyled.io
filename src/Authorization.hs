{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Authorization
    ( requireRepositoryAccess
    , requireRepositoriesAccess
    , authorizeAdmin
    , authorizeRepo
    ) where

import Import.NoFoundation

import Database.Persist.Sql (SqlReadT)
import GitHub.Data (Name, toPathPart)
import qualified GitHub.Data as GH
import Model.Collaborator

-- | Require the current user has access to the given @'Repo'@
--
-- Right now this simply asserts it's public. Some day we may support mirroring
-- GitHub permissions, granting read to admins, etc.
--
requireRepositoryAccess :: MonadHandler m => Repo -> SqlReadT m ()
requireRepositoryAccess Repo {..} | not repoIsPrivate = pure ()
requireRepositoryAccess _ = notFound -- Don't leak existence

-- | Run @'requireRepositoryAccess'@ on all @'Repo'@s in the list
requireRepositoriesAccess :: MonadHandler m => [Entity Repo] -> SqlReadT m ()
requireRepositoriesAccess = traverse_ $ requireRepositoryAccess . entityVal

authorizeAdmin :: AppSettings -> Maybe UserId -> DB AuthResult
authorizeAdmin _ Nothing = notFound
authorizeAdmin settings (Just userId) = do
    user <- fromMaybeM notFound =<< get userId
    authorizeWhen $ userEmail user `elem` appAdmins settings

authorizeRepo
    :: AppSettings
    -> Name GH.Owner
    -> Name GH.Repo
    -> Maybe UserId
    -> DB AuthResult
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
        <> " for authenticated UserId: "
        <> tshow userId

    if repoIsPrivate repo
        then do
            user <- get404 userId
            canRead <- collaboratorCanRead
                (appGitHubAppId settings)
                (appGitHubAppKey settings)
                repo
                user
            logDebugN $ "User: " <> tshow user
            logDebugN $ "Can-Read: " <> tshow canRead
            authorizeWhen canRead
        else pure Authorized

authorizeWhen :: MonadHandler m => Bool -> m AuthResult
authorizeWhen True = pure Authorized
authorizeWhen False = notFound
