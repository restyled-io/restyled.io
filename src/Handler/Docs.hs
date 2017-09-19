{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Docs where

import Import

getDocsR :: Handler Html
getDocsR = do
    defaultLayout $ do
        setTitle "Restyled - Documentation"
        $(widgetFile "docs")
