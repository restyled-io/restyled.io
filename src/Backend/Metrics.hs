{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Metrics
    ( jobAttempted
    , jobSucceeded
    , jobFailed
    ) where

import Import

import Backend.Foundation
import Model.AppMetrics

jobAttempted :: MonadBackend m => m ()
jobAttempted = do
    AppMetrics {..} <- asks backendMetrics
    increment amJobAttempted

jobSucceeded :: MonadBackend m => m ()
jobSucceeded = do
    AppMetrics {..} <- asks backendMetrics
    increment amJobSucceeded

jobFailed :: MonadBackend m => m ()
jobFailed = do
    AppMetrics {..} <- asks backendMetrics
    increment amJobFailed
