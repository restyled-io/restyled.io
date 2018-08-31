{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Collaborator
    ( collaboratorCanRead
    ) where

import Import.NoFoundation

import Control.Error.Util (note)
import Control.Monad.Except
import SVCS.GitHub

-- | See if a Collaborator has Read-access to a Repository (VCS-agnostic)
collaboratorCanRead
    :: (MonadIO m, MonadLogger m)
    => AppSettings
    -> Entity Repo
    -> User
    -> m Bool
collaboratorCanRead settings repo@(Entity _ Repo {..}) User {..} = do
    result <- runExceptT $ do
        username <- liftEither $ note "No GitHub username" userGithubUsername
        token <- ExceptT $ repoAccessToken settings repo
        githubCollaboratorCanRead token repoOwner repoName username

    either err pure result
  where
    err e = logErrorN ("Error authorizing repository:\n" <> pack e) $> False
