{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Repo
    ( findOrCreateRepo
    , RepoWithStats(..)
    , repoWithStats
    )
where

import ClassyPrelude

import Database.Persist.Sql
import GitHub.Data hiding (Repo(..), User(..))
import qualified GitHub.Data as GH
import GitHub.Data.Apps hiding (installationId)
import Model

findOrCreateRepo :: GH.Repo -> Id Installation -> DB (Entity Repo)
findOrCreateRepo ghRepo installationId = do
    let
        repo = Repo
            { repoOwner = simpleOwnerLogin $ GH.repoOwner ghRepo
            , repoName = GH.repoName ghRepo
            , repoInstallationId = installationId
            , repoIsPrivate = GH.repoPrivate ghRepo
            , repoDebugEnabled = False
            }

    upsert
        repo
        [ RepoInstallationId =. repoInstallationId repo
        , RepoIsPrivate =. repoIsPrivate repo
        ]

data RepoWithStats = RepoWithStats
    { rwsRepo :: Entity Repo
    , rwsJobCount :: Int
    , rwsErrorCount :: Int
    , rwsLastJob :: Maybe (Entity Job)
    }

repoWithStats :: Entity Repo -> DB RepoWithStats
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
