{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Admin.Jobs
    ( getAdminJobsNewR
    , postAdminJobsR
    ) where

import Import

import Backend.Foundation (runBackendHandler)
import Backend.Job
import GitHub.Data (toPathPart)
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
        FormSuccess CreateJob{..} -> do
            mRepo <- runDB $ getBy $ UniqueRepo cjOwner cjRepo

            for_ mRepo $ \repo -> do
                job <- runDB $ insertJob repo cjPullRequest
                runBackendHandler $ enqueueRestylerJob job
                setMessage "Job created"
                redirect $ AdminP $ AdminJobsP AdminJobsR

            setMessage $ toHtml
                $ "Unknown Repo: "
                <> toPathPart cjOwner <> "/"
                <> toPathPart cjRepo

        _ -> setMessage "Error creating Job"

    -- If we get here, we're rendering with an error
    adminLayout $ do
        setTitle "Restyled Admin / New Job"
        $(widgetFile "admin/jobs/new")
