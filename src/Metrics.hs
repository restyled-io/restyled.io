{-# LANGUAGE RecordWildCards #-}

module Metrics
    ( webhookReceived
    ) where

import Import

import Model.AppMetrics

webhookReceived :: Handler ()
webhookReceived = do
    AppMetrics {..} <- getsYesod appMetrics
    increment amWebhookReceived
