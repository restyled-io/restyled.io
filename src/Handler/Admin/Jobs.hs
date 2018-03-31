{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Jobs
    ( getAdminJobsR
    , getAdminJobsNewR
    , postAdminJobsR
    ) where

import Import

import Backend.Foundation (runBackendHandler)
import Backend.Job (enqueueRestylerJob)
import Widgets.Job

getAdminJobsR :: Handler Html
getAdminJobsR = do
    jobs <- runDB $ selectList [] [Desc JobCreatedAt]

    adminLayout $ do
        setTitle "Restyled Admin / Jobs"
        $(widgetFile "admin/jobs")

getAdminJobsNewR :: Handler Html
getAdminJobsNewR = do
    (widget, enctype) <- generateFormPost createJobForm

    adminLayout $ do
        setTitle "Restyled Admin / New Job"
        $(widgetFile "admin/jobs/new")

postAdminJobsR :: Handler Html
postAdminJobsR = do
    ((result, widget), enctype) <- runFormPost createJobForm

    case result of
        FormSuccess CreateJob{..} -> do
            now <- liftIO getCurrentTime
            job <- runDB
                $ insertEntity Job
                    { jobInstallationId = cjInstallationId
                    , jobOwner = cjOwner
                    , jobRepo = cjRepo
                    , jobPullRequest = cjPullRequest
                    , jobCreatedAt = now
                    , jobUpdatedAt = now
                    , jobCompletedAt = Nothing
                    , jobExitCode = Nothing
                    , jobStdout = Nothing
                    , jobStderr = Nothing
                    }
            runBackendHandler $ enqueueRestylerJob job
            setMessage "Job created"
            redirect $ AdminP $ AdminJobsP AdminJobsR

        _ -> do
            setMessage "Error creating Job"
            adminLayout $ do
                setTitle "Restyled Admin / New Job"
                $(widgetFile "admin/jobs/new")
