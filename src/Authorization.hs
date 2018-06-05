{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Authorization
    ( authorizeAdmin
    , authorizeRepo
    ) where

import Import.NoFoundation

import GitHub.Data (Name, toPathPart)
import qualified GitHub.Data as GH
import Model.Collaborator

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
        <> toPathPiece userId

    if repoIsPrivate repo
        then do
            user <- get404 userId
            canRead <- collaboratorCanRead settings repo user
            logDebugN $ "User: " <> tshow user
            logDebugN $ "Can-Read: " <> tshow canRead
            authorizeWhen canRead
        else pure Authorized

authorizeWhen :: MonadHandler m => Bool -> m AuthResult
authorizeWhen True = pure Authorized
authorizeWhen False = notFound
