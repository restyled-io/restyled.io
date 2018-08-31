{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Repo
    ( RepoWithStats(..)
    , repoWithStats
    , IgnoredWebhookReason(..)
    , initializeFromWebhook
    , RepoAccessToken(..)
    , repoAccessToken
    )
where

import ClassyPrelude

import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Model
import Settings
import SVCS.GitHub

data RepoWithStats = RepoWithStats
    { rwsRepo :: Entity Repo
    , rwsJobCount :: Int
    , rwsErrorCount :: Int
    , rwsLastJob :: Maybe (Entity Job)
    }

repoWithStats :: MonadIO m => Entity Repo -> SqlPersistT m RepoWithStats
repoWithStats repo =
    RepoWithStats repo
        <$> count
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                ]
        <*> count
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                , JobExitCode !=. Just 0
                , JobExitCode !=. Nothing
                ]
        <*> selectFirst
                [ JobOwner ==. repoOwner (entityVal repo)
                , JobRepo ==. repoName (entityVal repo)
                , JobCompletedAt !=. Nothing
                ]
                [Desc JobCreatedAt]

data IgnoredWebhookReason
    = IgnoredAction PullRequestEventType
    | IgnoredEventType Text
    | OwnPullRequest Text
    | PrivateNoPlan OwnerName RepoName

initializeFromWebhook
    :: MonadIO m
    => Payload
    -> SqlPersistT m (Either IgnoredWebhookReason (Entity Repo))
initializeFromWebhook payload@Payload {..}
    | pAction `notElem` enqueueEvents = pure $ Left $ IgnoredAction pAction
    | not $ isActualAuthor pAuthor = pure $ Left $ OwnPullRequest pAuthor
    | otherwise = Right <$> findOrCreateRepo payload

findOrCreateRepo :: MonadIO m => Payload -> SqlPersistT m (Entity Repo)
findOrCreateRepo Payload {..} = do
    let
        repo = Repo
            { repoOwner = pOwnerName
            , repoName = pRepoName
            , repoInstallationId = pInstallationId
            , repoIsPrivate = pRepoIsPrivate
            , repoDebugEnabled = False
            }

    upsert
        repo
        [ RepoInstallationId =. repoInstallationId repo
        , RepoIsPrivate =. repoIsPrivate repo
        ]

isActualAuthor :: Text -> Bool
isActualAuthor author
    | "restyled-io" `isPrefixOf` author = False
    | "[bot]" `isSuffixOf` author = False
    | otherwise = True

enqueueEvents :: [PullRequestEventType]
enqueueEvents = [PullRequestOpened, PullRequestSynchronized]

-- | Get an AccessToken for a Repository (VCS-agnostic)
repoAccessToken
    :: MonadIO m
    => AppSettings
    -> Entity Repo
    -> m (Either String RepoAccessToken)
repoAccessToken AppSettings {..} (Entity _ Repo {..}) =
    liftIO
        $ fmap (RepoAccessToken . atToken)
        <$> installationAccessToken
                appGitHubAppId
                appGitHubAppKey
                repoInstallationId
