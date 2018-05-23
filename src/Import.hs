{-# LANGUAGE RecordWildCards #-}

module Import
    ( module Import
    ) where

import Foundation as Import
import Import.NoFoundation as Import

fromMaybeM :: Applicative m => m a -> Maybe a -> m a
fromMaybeM d = maybe d pure

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
f <$$> a = fmap f <$> a

repoJobsRoute :: Entity Repo -> Route App
repoJobsRoute (Entity _ Repo {..}) =
    OwnerP repoOwner $ ReposP $ RepoP repoName $ RepoJobsP RepoJobsR

repoJobRoute :: Entity Job -> Route App
repoJobRoute (Entity jobId Job {..}) =
    OwnerP jobOwner $ ReposP $ RepoP jobRepo $ RepoJobsP $ RepoJobR jobId
