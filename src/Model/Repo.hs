{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Repo
    ( repoPath
    , repoPullPath
    , repoAccessToken
    , RepoWithStats(..)
    , repoWithStats
    , IgnoredWebhookReason(..)
    , initializeFromWebhook
    , findRepoOwner
    )
where

import ClassyPrelude

import Control.Error.Util (note)
import Control.Monad.Except
import Database.Persist
import Database.Persist.Sql (SqlPersistT)
import Model
import Settings
import SVCS.GitHub
import SVCS.GitLab
import Yesod.Core (toPathPiece)

-- | Make a nicely-formatted @:owner\/:name@
--
-- Surprisingly, this can be valuable to have a shorter name available
--
repoPath :: OwnerName -> RepoName -> Text
repoPath owner name = toPathPiece owner <> "/" <> toPathPiece name

repoPullPath :: OwnerName -> RepoName -> PullRequestNum -> Text
repoPullPath owner name num = repoPath owner name <> "#" <> toPathPiece num

-- | Get an AccessToken for a Repository, as appopriate for its SVCS
repoAccessToken
    :: MonadIO m
    => AppSettings
    -> Entity Repo
    -> Maybe (Entity User)
    -> m (Either String RepoAccessToken)
repoAccessToken AppSettings {..} (Entity _ Repo {..}) mUser =
    liftIO $ case repoSvcs of
        GitHubSVCS -> githubInstallationToken
            appGitHubAppId
            appGitHubAppKey
            repoInstallationId
        GitLabSVCS -> runExceptT $ do
            -- Until we truly support GitLab, these error scenarios are very
            -- unlikely outside of my own testing. So let's be terse.
            oauthKeys <- noteE "No GitLab OAuth2 keys" appGitLabOAuthKeys
            Entity _ User {..} <- noteE "GitLab requires user" mUser
            refreshToken <- noteE "No refreshToken" userGitlabRefreshToken
            ExceptT $ liftIO $ gitlabRefreshedToken oauthKeys refreshToken
  where
    noteE :: Monad m => e -> Maybe a -> ExceptT e m a
    noteE msg = liftEither . note msg

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
            { repoSvcs = pSVCS
            , repoOwner = pOwnerName
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

findRepoOwner :: MonadIO m => Entity Repo -> SqlPersistT m (Maybe (Entity User))
findRepoOwner (Entity _ Repo {..}) = selectFirst filters []
  where
    filters = case repoSvcs of
        GitHubSVCS ->
            [UserGithubUsername ==. Just (githubOwnerToUser repoOwner)]
        GitLabSVCS ->
            [UserGitlabUsername ==. Just (gitlabOwnerToUser repoOwner)]
