{-# LANGUAGE TemplateHaskell #-}

module Handler.Home
    ( getHomeR
    )
where

import Import

import Foundation
import Yesod

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Restyled"
    $(widgetFile "homepage")
