{-# LANGUAGE TemplateHaskell #-}

-- | All repos in the system
module Restyled.Handlers.Admin.Repos
    ( getAdminRepoR
    , patchAdminRepoR
    , getAdminReposSearchR
    ) where

import Restyled.Prelude

import Restyled.Admin.RepoSearch
import Restyled.Foundation
import Restyled.Models
import Restyled.RestylerImage hiding (restylerImage)
import Restyled.Routes
import Restyled.Settings
import Restyled.Yesod

getAdminRepoR :: RepoId -> Handler Value
getAdminRepoR repoId = runDB $ toJSON <$> getEntity404 repoId

data AdminRepoPatch = AdminRepoPatch
    { debugEnabled :: Maybe Bool
    , enabled :: Maybe Bool
    , restylerImage :: Maybe (Maybe RestylerImage)
    }

-- brittany-disable-next-binding

instance FromJSON AdminRepoPatch where
    -- Custom instance needed for double-Maybe
    parseJSON = withObject "AdminRepoPatch" $ \o ->
        AdminRepoPatch
            <$> o .:? "debugEnabled"
            <*> o .:? "enabled"
            <*> o .:! "restylerImage"

patchAdminRepoR :: RepoId -> Handler Value
patchAdminRepoR repoId = do
    AdminRepoPatch {..} <- requireCheckJsonBody

    runDB $ do
        update repoId $ catMaybes
            [ (RepoDebugEnabled =.) <$> debugEnabled
            , (RepoEnabled =.) <$> enabled
            , (RepoRestylerImage =.) <$> restylerImage
            ]

        toJSON <$> getEntity404 repoId

getAdminReposSearchR :: Handler TypedContent
getAdminReposSearchR = do
    mQuery <- runInputGet $ iopt textField "q"
    results <- maybe (pure noResults) (searchRepos 10) mQuery

    selectRep $ do
        provideRep $ pure $ toJSON results
        provideRep $ adminLayout $ do
            setTitle "Restyled Admin / Search"
            $(widgetFile "admin/repos/search")
