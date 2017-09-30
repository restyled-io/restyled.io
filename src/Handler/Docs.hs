{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Docs where

import Import

import Helpers.Markdown

getDocsR :: Handler Html
getDocsR = do
    urlRender <- getUrlRender

    defaultLayout $ do
        setTitle "Restyled - Documentation"
        markdownFromTemplate $(textFile "templates/docs.md") urlRender
