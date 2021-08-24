{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Repos
    ( getAdminReposSearchR
    ) where

import Restyled.Prelude

import Restyled.Admin.RepoSearch
import qualified Restyled.Api.Repo as ApiRepo
import Restyled.Foundation
import Restyled.Routes
import Restyled.Settings
import Restyled.Yesod

getAdminReposSearchR :: Handler TypedContent
getAdminReposSearchR = do
    mQuery <- runInputGet $ iopt textField "q"
    results <- maybe (pure noResults) (searchRepos 10) mQuery

    selectRep $ do
        provideRep $ pure $ toJSON results
        provideRep $ adminLayout $ do
            setTitle "Restyled Admin / Search"
            $(widgetFile "admin/repos/search")
