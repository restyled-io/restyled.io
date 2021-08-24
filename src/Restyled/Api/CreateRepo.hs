{-# LANGUAGE NamedFieldPuns #-}

module Restyled.Api.CreateRepo
    ( ApiCreateRepoErrors
    , ApiCreateRepoError
    , assertOwnerName
    , assertRepoName
    , findOrCreateRepo
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Api.Repo (ApiRepo(ApiRepo), apiRepo)
import qualified Restyled.Api.Repo as ApiRepo
import Restyled.Marketplace (marketplacePlanAllows)
import Restyled.Models.DB
import Restyled.Models.Repo (upsertRepo)

newtype ApiCreateRepoErrors = ApiCreateRepoErrors
    { errors :: NonEmpty ApiCreateRepoError
    }
    deriving stock (Eq, Show, Generic)
    deriving newtype Semigroup
    deriving anyclass ToJSON

apiCreateRepoError :: ApiCreateRepoError -> ApiCreateRepoErrors
apiCreateRepoError = ApiCreateRepoErrors . pure

data ApiCreateRepoError
    = UnexpectedOwnerName (Expected OwnerName)
    | UnexpectedRepoName (Expected RepoName)
    deriving stock (Eq, Show, Generic)
    deriving anyclass ToJSON

newtype Expected a = Expected
    { expected :: a
    }
    deriving stock (Eq, Show, Generic)
    deriving anyclass ToJSON

unexpectedOwnerName :: OwnerName -> ApiCreateRepoErrors
unexpectedOwnerName = apiCreateRepoError . UnexpectedOwnerName . Expected

unexpectedRepoName :: RepoName -> ApiCreateRepoErrors
unexpectedRepoName = apiCreateRepoError . UnexpectedRepoName . Expected

assertOwnerName
    :: Monad m => OwnerName -> ApiRepo -> ValidateT ApiCreateRepoErrors m ()
assertOwnerName expectedOwner ApiRepo { owner }
    | owner /= expectedOwner = refute $ unexpectedOwnerName expectedOwner
    | otherwise = pure ()

assertRepoName
    :: Monad m => RepoName -> ApiRepo -> ValidateT ApiCreateRepoErrors m ()
assertRepoName expectedName ApiRepo { name }
    | name /= expectedName = refute $ unexpectedRepoName expectedName
    | otherwise = pure ()

findOrCreateRepo
    :: MonadIO m
    => ApiRepo
    -> ValidateT ApiCreateRepoErrors (SqlPersistT m) ApiRepo
findOrCreateRepo body = lift $ do
    repo <- upsertRepo Repo
        { repoSvcs = GitHubSVCS
        , repoOwner = ApiRepo.owner body
        , repoName = ApiRepo.name body
        , repoInstallationId = ApiRepo.installationId body
        , repoIsPrivate = ApiRepo.isPrivate body
        , repoDebugEnabled = False
        , repoEnabled = True
        , repoRestylerImage = Nothing
        }

    apiRepo repo . Just <$> marketplacePlanAllows repo
