{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Model.Repo
    ( findOrCreateRepo
    ) where

import ClassyPrelude

import Database.Persist.Sql
import GitHub.Data hiding (Repo(..))
import qualified GitHub.Data as GH
import GitHub.Data.Apps
import Model

findOrCreateRepo :: GH.Repo -> Id Installation -> DB (Entity Repo)
findOrCreateRepo ghRepo installationId = do
    let repo = Repo
            { repoOwner = simpleOwnerLogin $ GH.repoOwner ghRepo
            , repoName = GH.repoName ghRepo
            , repoInstallationId = installationId
            , repoIsPrivate = GH.repoPrivate ghRepo
            , repoDebugEnabled = False
            }

    upsert repo
        [ RepoInstallationId =. repoInstallationId repo
        , RepoIsPrivate =. repoIsPrivate repo
        ]
