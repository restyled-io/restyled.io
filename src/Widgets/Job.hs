{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets.Job
    ( jobCard
    , jobOutput

    -- * Job completion
    -- |
    --
    -- Needed in @"Widgets.Repo"@
    --
    , Completion(..)
    , jobCompletion

    -- * Admin widget
    , adminJobCard

    -- * Creating Jobs
    , CreateJob(..)
    , createJobForm
    ) where

import Import

import qualified Data.Text as T
import Formatting (format)
import Formatting.Time (diff)

data CreateJob = CreateJob
    { cjSvcs :: RepoSVCS
    , cjOwner :: OwnerName
    , cjRepo :: RepoName
    , cjPullRequest :: PullRequestNum
    }

-- | Form to use when rendering for real input, or processing
createJobForm :: Form CreateJob
createJobForm =
    renderDivs
        $ CreateJob
        <$> areq svcsSelectField "SVCS" Nothing
        <*> (mkOwnerName <$> areq textField "Owner" Nothing)
        <*> (mkRepoName <$> areq textField "Repo" Nothing)
        <*> (mkPullRequestNum <$> areq intField "Pull Request" Nothing)

svcsSelectField :: Field (HandlerFor App) RepoSVCS
svcsSelectField = selectField $ pure optionList
  where
    optionList :: OptionList RepoSVCS
    optionList = OptionList
        { olOptions = map toOption [minBound .. maxBound]
        , olReadExternal = fromPathPiece
        }

    toOption :: RepoSVCS -> Option RepoSVCS
    toOption svcs = Option
        { optionDisplay = showRepoSVCS svcs
        , optionInternalValue = svcs
        , optionExternalValue = toPathPiece svcs
        }

--- | Form to use when re-submitting an existing @'Job'@
createJobFormFrom :: Job -> Form CreateJob
createJobFormFrom Job {..} =
    renderDivs
        $ CreateJob
        <$> areq hiddenField "" (Just jobSvcs)
        <*> areq hiddenField "" (Just jobOwner)
        <*> areq hiddenField "" (Just jobRepo)
        <*> areq hiddenField "" (Just jobPullRequest)

-- | Internal helper for rendering completion state
data Completion
    = Success UTCTime
    | Failure UTCTime Int
    | InProgress

jobCompletion :: Job -> Completion
jobCompletion job = case (jobCompletedAt job, jobExitCode job) of
    (Just completedAt, Just 0) -> Success completedAt
    (Just completedAt, Just n) -> Failure completedAt n
    _ -> InProgress

jobCard :: Entity Job -> Widget
jobCard job = do
    now <- liftIO getCurrentTime

    let mActions :: Maybe Widget
        mActions = Nothing

    $(widgetFile "widgets/job-card")

-- brittany-disable-next-binding

jobOutput :: Job -> Widget
jobOutput job = $(widgetFile "widgets/job-output")

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

    let mActions :: Maybe Widget
        mActions = Just $ adminJobActions job

    $(widgetFile "widgets/job-card")

-- brittany-disable-next-binding

adminJobActions :: Entity Job -> Widget
adminJobActions job = do
    (widget, enctype) <- handlerToWidget
         $ generateFormPost
         $ createJobFormFrom
         $ entityVal job

    [whamlet|
        $maybe _ <- jobCompletedAt $ entityVal job
            <form method=post action=@{adminJobsP AdminJobsR} enctype=#{enctype}>
                ^{widget}
                <button .action>Rerun job

        <form method=post action=@{adminJobsP $ AdminJobR $ entityKey job}>
            <input type=hidden name=_method value=DELETE />
            <button .warning>Delete job
    |]
