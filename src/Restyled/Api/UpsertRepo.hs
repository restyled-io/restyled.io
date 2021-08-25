{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Api.UpsertRepo
    ( ApiUpsertRepo(..)
    , ApiUpsertRepoErrors
    , ApiUpsertRepoError
    , assertOwnerName
    , assertRepoName
    , upsertRepo
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Api.Repo (ApiRepo, apiRepo)
import Restyled.Marketplace (marketplacePlanAllows)
import Restyled.Models.DB
import qualified Restyled.Models.Repo as Repo
import Restyled.Settings (HasSettings(..))

newtype ApiUpsertRepoErrors = ApiUpsertRepoErrors
    { errors :: NonEmpty ApiUpsertRepoError
    }
    deriving stock (Eq, Show, Generic)
    deriving newtype Semigroup
    deriving anyclass ToJSON

apiCreateRepoError :: ApiUpsertRepoError -> ApiUpsertRepoErrors
apiCreateRepoError = ApiUpsertRepoErrors . pure

data ApiUpsertRepoError
    = UnexpectedOwnerName (Expected OwnerName)
    | UnexpectedRepoName (Expected RepoName)
    deriving stock (Eq, Show, Generic)
    deriving anyclass ToJSON

newtype Expected a = Expected
    { expected :: a
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass ToJSON

unexpectedOwnerName :: OwnerName -> ApiUpsertRepoErrors
unexpectedOwnerName = apiCreateRepoError . UnexpectedOwnerName . Expected

unexpectedRepoName :: RepoName -> ApiUpsertRepoErrors
unexpectedRepoName = apiCreateRepoError . UnexpectedRepoName . Expected

assertOwnerName
    :: Monad m
    => OwnerName
    -> ApiUpsertRepo
    -> ValidateT ApiUpsertRepoErrors m ()
assertOwnerName expectedOwner ApiUpsertRepo { owner }
    | owner /= expectedOwner = refute $ unexpectedOwnerName expectedOwner
    | otherwise = pure ()

assertRepoName
    :: Monad m
    => RepoName
    -> ApiUpsertRepo
    -> ValidateT ApiUpsertRepoErrors m ()
assertRepoName expectedName ApiUpsertRepo { name }
    | name /= expectedName = refute $ unexpectedRepoName expectedName
    | otherwise = pure ()

data ApiUpsertRepo = ApiUpsertRepo
    { owner :: OwnerName
    , name :: RepoName
    , isPrivate :: Bool
    , installationId :: InstallationId
    }
    deriving stock Generic
    deriving anyclass FromJSON

upsertRepo
    :: (MonadIO m, MonadReader env m, HasSettings env)
    => ApiUpsertRepo
    -> ValidateT ApiUpsertRepoErrors (SqlPersistT m) ApiRepo
upsertRepo ApiUpsertRepo { owner, name, isPrivate, installationId } = lift $ do
    repo <- Repo.upsertRepo Repo
        { repoSvcs = GitHubSVCS
        , repoOwner = owner
        , repoName = name
        , repoInstallationId = installationId
        , repoIsPrivate = isPrivate
        , repoDebugEnabled = False
        , repoEnabled = True
        , repoRestylerImage = Nothing
        }

    settings <- lift $ view settingsL
    apiRepo repo settings . Just <$> marketplacePlanAllows repo
