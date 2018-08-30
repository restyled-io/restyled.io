{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Thanks
    ( getThanksGitHubR
    ) where

import Import

-- brittany-disable-next-binding

getThanksGitHubR :: Handler Html
getThanksGitHubR = defaultLayout $ do
    setTitle "Thanks"
    $(widgetFile "thanks/github")
