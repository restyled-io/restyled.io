{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Home
    ( getHomeR
    )
where

import Import

import Forms

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost signupForm

    defaultLayout $ do
        setTitle "Restyled"
        $(widgetFile "homepage")
