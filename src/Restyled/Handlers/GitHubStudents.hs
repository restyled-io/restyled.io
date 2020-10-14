{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.GitHubStudents
    ( getGitHubStudentsR
    , getGitHubStudentsThanksR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.GitHubStudents
import Restyled.Marketplace
import Restyled.Models
import Restyled.Settings
import Restyled.Yesod

getGitHubStudentsR :: Handler Html
getGitHubStudentsR = do
    setUltDest $ GitHubStudentsP GitHubStudentsThanksR

    defaultLayout $ do
        setTitle "Restyled for GitHub Students"
        $(widgetFile "github-students")

getGitHubStudentsThanksR :: Handler Html
getGitHubStudentsThanksR = do
    -- Hidden parameter so we can test both views
    fakeVerified <- fmap (fromMaybe False) $ runInputGet $ iopt
        boolField
        "fake-verified"

    wasGifted <- runDB $ (fakeVerified ||) <$> do
        mUser <- entityVal <$$> maybeAuth
        planId <- entityKey <$> findOrCreateMarketplacePlan githubStudentsPlan
        let mLogin = userGithubUsername =<< mUser
        maybe (pure False) (fetchUserHasMarketplacePlan planId) mLogin

    defaultLayout $ do
        setTitle "Restyled for GitHub Students"
        $(widgetFile "github-students/thanks")
