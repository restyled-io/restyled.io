{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Forms

getHomeR :: Handler Html
getHomeR = do
    (widget, enctype) <- generateFormPost signupForm

    defaultLayout $ do
        setTitle "Restyled"
        $(widgetFile "homepage")
