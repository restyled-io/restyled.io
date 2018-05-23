{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Widgets.Job
    ( jobCard
    , jobOutput

    -- * Creating Jobs
    , CreateJob(..)
    , createJobForm
    , createJobFormFromRepo
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

-- | Form to use when submitting a @'Job'@ for a known @'Repo'@
createJobFormFromRepo :: Repo -> Form CreateJob
createJobFormFromRepo Repo{..} = renderDivs $ CreateJob
    <$> areq hiddenField "" (Just repoOwner)
    <*> areq hiddenField "" (Just repoName)
    <*> (mkId Proxy <$> areq intField ("" { fsAttrs = attrs })Nothing)
  where
    attrs =
        [ ("placeholder", "PR Number")
        ]

-- | Internal helper for rendering completion state
data Completion
    = Success UTCTime
    | Failure UTCTime Int
    | InProgress

jobCompletion :: Job -> Completion
jobCompletion job =
    case (jobCompletedAt job, jobExitCode job) of
        (Just completedAt, Just 0) -> Success completedAt
        (Just completedAt, Just n) -> Failure completedAt n
        _ -> InProgress

jobCard :: Entity Job -> Widget
jobCard job = do
    now <- liftIO getCurrentTime
    $(widgetFile "widgets/job-card")

jobOutput :: Job -> Widget
jobOutput job =
    $(widgetFile "widgets/job-output")
