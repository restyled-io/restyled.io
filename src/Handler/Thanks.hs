{-# LANGUAGE TemplateHaskell #-}

module Handler.Thanks
    ( getThanksGitHubR
    , getThanksGitHubSetupR
    )
where

import Import

import Foundation
import Yesod
import Yesod.Auth.OAuth2

-- brittany-disable-next-binding

getThanksGitHubR :: Handler Html
getThanksGitHubR = defaultLayout $ do
    setTitle "Thanks"
    $(widgetFile "thanks/github")

getThanksGitHubSetupR :: Handler ()
getThanksGitHubSetupR = do
    setUltDest ThanksGitHubR
    redirect $ AuthR $ oauth2Url "github"
