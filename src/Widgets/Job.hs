{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Widgets.Job
    ( jobsTable
    , jobsTableRow

    -- * Creating Jobs
    , CreateJob(..)
    , createJobForm
    , createJobFormFrom
    ) where

import Import

import Data.Time
import Formatting (format)
import Formatting.Time (diff)
import GitHub.Data hiding (Repo)
import qualified GitHub.Data as GH

data CreateJob = CreateJob
    { cjOwner :: Name Owner
    , cjRepo :: Name GH.Repo
    , cjPullRequest :: Id PullRequest
    }

-- | Form to use when rendering for real input, or processing
createJobForm :: Form CreateJob
createJobForm = renderDivs $ CreateJob
    <$> (mkName Proxy <$> areq textField "Owner" Nothing)
    <*> (mkName Proxy <$> areq textField "Repo" Nothing)
    <*> (mkId Proxy <$> areq intField "Pull Request" Nothing)

-- | Form to use when re-submitting an existing @'Job'@
createJobFormFrom :: Job -> Form CreateJob
createJobFormFrom Job{..} = renderDivs $ CreateJob
    <$> areq hiddenField "" (Just jobOwner)
    <*> areq hiddenField "" (Just jobRepo)
    <*> areq hiddenField "" (Just jobPullRequest)

-- | Form to use when submitting a @'Job'@ for a known @'Repo'@
createJobFormFromRepo :: Repo -> Form CreateJob
createJobFormFromRepo Repo{..} = renderDivs $ CreateJob
    <$> areq hiddenField "" (Just repoOwner)
    <*> areq hiddenField "" (Just repoName)
    <*> (mkId Proxy <$> areq intField "" Nothing)

jobsTable :: [Entity Job] -> Maybe Repo -> Widget
jobsTable jobs mRepo = do
    mForm <- for mRepo $ \repo ->
        handlerToWidget
            $ generateFormPost
            $ createJobFormFromRepo repo

    $(widgetFile "widgets/jobs-table")

jobsTableRow :: Entity Job -> Widget
jobsTableRow job = do
    now <- liftIO getCurrentTime
    (widget, enctype) <- handlerToWidget
        $ generateFormPost
        $ createJobFormFrom
        $ entityVal job
    $(widgetFile "widgets/jobs-table-row")

prefixLines :: Text -> Text -> Text
prefixLines prefix = unlines . map (prefix <>) . lines
