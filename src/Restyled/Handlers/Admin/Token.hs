{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Token
    ( getAdminTokensR
    , postAdminTokensR
    , deleteAdminTokenR
    ) where

import Restyled.Prelude

import Formatting (format)
import Formatting.Time (diff)
import Restyled.ApiToken
import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.Yesod

getAdminTokensR :: Handler Html
getAdminTokensR = do
    now <- liftIO getCurrentTime
    userId <- requireAuthId
    apiTokens <- runDB $ selectList
        [ApiTokenUser ==. userId]
        [ Desc ApiTokenLastUsedAt
        , Desc ApiTokenCreatedAt
        , Asc ApiTokenDescription
        ]

    adminLayout $ do
        setTitle "Admin - API Tokens"
        $(widgetFile "admin/tokens")

postAdminTokensR :: Handler Html
postAdminTokensR = do
    userId <- requireAuthId
    description <- runInputPost $ ireq textField "description"
    apiToken <- runDB $ createApiToken userId description
    setMessage
        $ toHtml
        $ "Your API Token is "
        <> apiTokenRaw apiToken
        <> ". Store it somewhere safe"
        <> ", this is the last time you'll see it."
    redirect $ AdminP $ AdminTokensP AdminTokensR

deleteAdminTokenR :: ApiTokenId -> Handler Html
deleteAdminTokenR apiTokenId = do
    userId <- requireAuthId
    runDB $ do
        void $ fromMaybeM notFound $ selectFirst
            [ApiTokenId ==. apiTokenId, ApiTokenUser ==. userId]
            []
        delete apiTokenId
    redirect $ AdminP $ AdminTokensP AdminTokensR
