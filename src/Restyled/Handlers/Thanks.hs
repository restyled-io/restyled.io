{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Thanks
    ( getThanksGitHubR
    , getThanksGitHubSetupR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Settings
import Restyled.Yesod

-- brittany-disable-next-binding

getThanksGitHubR :: Handler Html
getThanksGitHubR = defaultLayout $ do
    setTitle "Thanks"
    $(widgetFile "thanks/github")

getThanksGitHubSetupR :: Handler ()
getThanksGitHubSetupR = do
    setUltDest ThanksGitHubR
    redirect $ AuthR $ oauth2Url "github"
