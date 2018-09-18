{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Admin.Jobs
    ( getAdminJobsNewR
    , postAdminJobsR
    , deleteAdminJobR
    )
where

import Import

import Backend.Foundation (runBackendHandler)
import Backend.Job
import Widgets.Job

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
        FormSuccess CreateJob {..} -> do
            mRepo <- runDB $ getBy $ UniqueRepo cjOwner cjRepo

            for_ mRepo $ \repo -> do
                job <- runDB $ insertJob repo cjPullRequest
                runBackendHandler $ enqueueRestylerJob job
                setMessage "Job created"
                redirect AdminR

            setMessage $ toHtml $ "Unknown Repo: " <> repoPath cjOwner cjRepo

        _ -> setMessage "Error creating Job"

    -- If we get here, we're rendering with an error
    adminLayout $ do
        setTitle "Restyled Admin / New Job"
        $(widgetFile "admin/jobs/new")

deleteAdminJobR :: JobId -> Handler Html
deleteAdminJobR jobId = do
    runDB $ do
        void $ get404 jobId
        delete jobId

    setMessage "Job deleted"
    redirect AdminR
