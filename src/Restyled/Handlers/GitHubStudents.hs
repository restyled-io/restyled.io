{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.GitHubStudents
    ( getGitHubStudentsR
    , getGitHubStudentsThanksR
    )
where

import Restyled.Prelude

import Restyled.Backend.DiscountMarketplacePlan
import Restyled.Foundation
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
        maybe (pure False) fetchUserHasDiscountMarketplacePlan mUser

    defaultLayout $ do
        setTitle "Restyled for GitHub Students"
        $(widgetFile "github-students/thanks")
