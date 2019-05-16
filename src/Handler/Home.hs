{-# LANGUAGE TemplateHaskell #-}

module Handler.Home
    ( getHomeR
    )
where

import Import

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
    setTitle "Restyled"
    $(widgetFile "homepage")
