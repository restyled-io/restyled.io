{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Admin.Repos
    ( getAdminReposR
    , getAdminReposSearchR
    , patchAdminRepoR
    , getAdminRepoJobsR
    )
where

import Import

import Data.Aeson
import Data.Aeson.Casing
import Widgets.Job
import Widgets.Repo
import Yesod.Paginator
import Yesod.Paginator.Instances ()

getAdminReposR :: Handler Html
getAdminReposR = do
    pages <- runDB $ do
        pages <- selectPaginated 5 [] [Desc RepoId]
        traverse repoWithStats pages

    adminLayout $ do
        setTitle "Restyled Admin / Repos"
        $(widgetFile "admin/repos")

data SearchResults
    = SearchResults
    { srRepos :: [Entity Repo]
    , srTotal :: Int
    }
    deriving Generic

instance ToJSON SearchResults where
    toJSON = genericToJSON $ aesonPrefix camelCase
    toEncoding = genericToEncoding $ aesonPrefix camelCase

getAdminReposSearchR :: Handler TypedContent
getAdminReposSearchR = do
    mQuery <- runInputGet $ iopt textField "q"

    results <- case mQuery of
        Nothing -> pure $ SearchResults [] 0
        Just q -> runDB $ do
            repos <- selectList (searchFilters q) [LimitTo 10]
            total <- if length repos == 10
                then count $ searchFilters q
                else pure $ length repos

            pure SearchResults {srRepos = repos, srTotal = total}

    selectRep $ do
        provideRep $ pure $ toJSON results
        provideRep $ adminLayout $ do
            setTitle "Restyled Admin / Search"
            $(widgetFile "admin/repos/search")

searchFilters :: Text -> [Filter Repo]
searchFilters q = [RepoOwner `ilike` q] ||. [RepoName `ilike` q]
  where
    ilike
        :: (IsString a, PersistField a)
        => EntityField record a
        -> Text
        -> Filter record
    ilike field value = Filter
        field
        (Left $ fromString $ unpack $ "%" <> value <> "%")
        (BackendSpecificFilter "ILIKE")

patchAdminRepoR :: RepoId -> Handler ()
patchAdminRepoR repoId = do
    debugEnabled <- runInputPost $ ireq boolField "debugEnabled"
    setMessage
        $ (if debugEnabled then "Enabled" else "Disabled")
        <> " debug for repository"
    runDB $ update repoId [RepoDebugEnabled =. debugEnabled]
    redirect $ AdminP $ AdminReposP AdminReposR

getAdminRepoJobsR :: RepoId -> Handler Html
getAdminRepoJobsR repoId = do
    (repo, jobs) <- runDB $ do
        repo@Repo {..} <- get404 repoId
        (repo, ) <$> selectList
            [JobOwner ==. repoOwner, JobRepo ==. repoName]
            [Desc JobCreatedAt]

    (widget, enctype) <- generateFormPost $ createJobFormFromRepo repo

    adminLayout $ do
        setTitle "Restyled Admin / Repo Jobs"
        $(widgetFile "admin/repos/jobs")
