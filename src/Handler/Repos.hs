{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Handler.Repos
    ( getRepoR
    , getRepoPullR
    , getRepoPullJobsR
    , getRepoJobsR
    , getRepoJobR
    )
where

import Import

import GitHub.Data hiding (Repo(..))
import qualified GitHub.Data as GH
import Widgets.Job

getRepoR :: Name Owner -> Name GH.Repo -> Handler Html
getRepoR = getRepoJobsR

getRepoPullR :: Name Owner -> Name GH.Repo -> Int -> Handler Html
getRepoPullR = getRepoPullJobsR

getRepoPullJobsR :: Name Owner -> Name GH.Repo -> Int -> Handler Html
getRepoPullJobsR owner name num = do
    jobs <- runDB $ selectList
        [ JobOwner ==. owner
        , JobRepo ==. name
        , JobPullRequest ==. mkId Proxy num
        ]
        [Desc JobCompletedAt, Desc JobCreatedAt]

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> "#"
            <> toPathPiece num
            <> " jobs"
        $(widgetFile "jobs")

getRepoJobsR :: Name Owner -> Name GH.Repo -> Handler Html
getRepoJobsR owner name = do
    jobs <- runDB $ selectList
        [JobOwner ==. owner, JobRepo ==. name]
        [Desc JobCompletedAt, Desc JobCreatedAt]

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> " jobs"
        $(widgetFile "jobs")

getRepoJobR :: Name Owner -> Name GH.Repo -> JobId -> Handler Html
getRepoJobR owner name jobId = do
    job <- runDB $ fromMaybeM notFound =<< getEntity jobId

    defaultLayout $ do
        setTitle
            $ toHtml
            $ toPathPart owner
            <> "/"
            <> toPathPart name
            <> " #"
            <> toPathPiece jobId
        $(widgetFile "job")
