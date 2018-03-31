{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Jobs
    ( getAdminJobsR
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

postAdminJobsR :: Handler ()
postAdminJobsR = do
    ((result, _widget), _enctype) <- runFormPost createJobForm

    case result of
        -- N.B. because we're not really rendering a form, the error messaging
        -- leaves a lot to be desired. But the reason we can skip the form
        -- (hard-coding default values from an existing Job) also means form
        -- validation errors are really only possible with programmer error
        FormMissing -> setMessage "Form data missing"
        FormFailure errs -> setMessage
            $ "Failure creating job: "
            <> toHtml (tshow errs)

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

    redirect $ AdminP AdminJobsR
