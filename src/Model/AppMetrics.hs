{-# LANGUAGE OverloadedStrings #-}

module Model.AppMetrics
    ( AppMetrics(..)
    , buildAppMetrics
    , increment

    -- * Monitoring threads
    , forkLocalhostServer
    , forkCloudWatchServer
    ) where

import Prelude

import Control.Monad (void)
import Control.Monad.IO.Class
import qualified Network.AWS as AWS
import System.Metrics (Store)
import qualified System.Metrics as EKG
import System.Metrics.Counter (Counter)
import qualified System.Metrics.Counter as EKG
import qualified System.Remote.Monitoring as EKG
import qualified System.Remote.Monitoring.CloudWatch as EKG

data AppMetrics = AppMetrics
    { amStore :: Store
    , amWebhookReceived :: Counter
    , amJobAttempted :: Counter
    , amJobSucceeded :: Counter
    , amJobFailed :: Counter
    }

-- | Build our system metric values
--
-- Naming convention
--
-- > {namespace}.{target: plural noun}.{action: past tense verb}
--
buildAppMetrics :: MonadIO m => m AppMetrics
buildAppMetrics = liftIO $ do
    store <- EKG.newStore
    EKG.registerGcMetrics store

    AppMetrics store
        <$> EKG.createCounter "app.webhooks.received" store
        <*> EKG.createCounter "backend.jobs.attempted" store
        <*> EKG.createCounter "backend.jobs.succeeded" store
        <*> EKG.createCounter "backend.jobs.failed" store

increment :: MonadIO m => Counter -> m ()
increment = liftIO . EKG.inc

forkLocalhostServer :: Store -> Int -> IO ()
forkLocalhostServer store = void . EKG.forkServerWith store "localhost"

forkCloudWatchServer :: Store -> IO ()
forkCloudWatchServer store = do
    env <- AWS.newEnv AWS.Discover
    void $ EKG.forkCloudWatch (EKG.defaultCloudWatchEnv "EKG" env) store
