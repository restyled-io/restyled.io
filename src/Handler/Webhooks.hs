{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Handler.Webhooks where

import Import
import Restyler
import Webhooks.GitHub.PullRequest

postWebhooksR :: Handler ()
postWebhooksR = do
    payload <- requireJsonBody
    $(logDebug) $ "Payload received: " <> tshow payload

    pullRequest <- runDB $ do
        orgId <- createOrUpdate $ toOrganization payload
        repoId <- createOrUpdate $ toRepository orgId payload
        createOrUpdateEntity $ toPullRequest repoId payload

    $(logDebug) $ "Restyling PR: " <> tshow pullRequest
    runRestyler pullRequest
