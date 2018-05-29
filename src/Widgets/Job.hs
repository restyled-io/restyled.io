{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.Job
    ( jobCard
    , jobOutput

    -- * Admin widget
    , adminJobCard

    -- * Creating Jobs
    , CreateJob(..)
    , createJobForm
    , createJobFormFromRepo
    ) where

import Import

import qualified Data.Text as T
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

--- | Form to use when re-submitting an existing @'Job'@
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

    let
        mActions :: Maybe Widget
        mActions = Nothing

    $(widgetFile "widgets/job-card")

jobOutput :: Job -> Widget
jobOutput job =
    $(widgetFile "widgets/job-output")

colorizedLogLine :: Text -> Widget
colorizedLogLine ln
    | Just message <- T.stripPrefix "[Debug] " ln = logLine "debug" message
    | Just message <- T.stripPrefix "[Info] " ln = logLine "info" message
    | Just message <- T.stripPrefix "[Warn] " ln = logLine "warn" message
    | Just message <- T.stripPrefix "[Error] " ln = logLine "error" message
    | otherwise = [whamlet|#{ln}|]
  where
    logLine :: Text -> Text -> Widget
    logLine level message = $(widgetFile "widgets/log-line")

-- | @'adminJobCard'@ just adds administrative actions to @'jobCard'@
adminJobCard :: Entity Job -> Widget
adminJobCard job = do
    now <- liftIO getCurrentTime

    let
        mActions :: Maybe Widget
        mActions = Just $ adminJobActions job

    $(widgetFile "widgets/job-card")

adminJobActions :: Entity Job -> Widget
adminJobActions job = do
    (widget, enctype) <- handlerToWidget
         $ generateFormPost
         $ createJobFormFrom
         $ entityVal job

    [whamlet|
        $maybe _ <- jobCompletedAt $ entityVal job
            <form method=post action=@{adminJobsRoute} enctype=#{enctype}>
                ^{widget}
                <button .action>Rerun job

        <form method=post action=@{adminJobRoute job}>
            <input type=hidden name=_method value=DELETE />
            <button .warning>Delete job
    |]
