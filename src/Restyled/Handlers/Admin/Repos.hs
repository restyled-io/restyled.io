{-# LANGUAGE TemplateHaskell #-}

-- | All repos in the system
module Restyled.Handlers.Admin.Repos
    ( getAdminReposR
    , getAdminReposSearchR
    )
where

import Restyled.Prelude

import Restyled.Admin.RepoSearch
import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.Widgets.Repo
import Restyled.Yesod

getAdminReposR :: Handler Html
getAdminReposR = do
    pages <- runDB $ do
        pages <- selectPaginated 5 [] [Desc RepoId]
        traverse repoWithStats pages

    adminLayout $ do
        setTitle "Restyled Admin / Repos"
        $(widgetFile "admin/repos")

getAdminReposSearchR :: Handler TypedContent
getAdminReposSearchR = do
    mQuery <- runInputGet $ iopt textField "q"
    results <- maybe (pure noResults) (searchRepos 10) mQuery

    selectRep $ do
        provideRep $ pure $ toJSON results
        provideRep $ adminLayout $ do
            setTitle "Restyled Admin / Search"
            $(widgetFile "admin/repos/search")
