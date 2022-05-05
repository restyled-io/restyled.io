module Restyled.Tracing
    ( SegmentName(..)
    , traceAppSegment

    -- * Re-exports
    , module Restyled.Tracing.App
    , module Restyled.Tracing.TransactionId
    ) where

import RIO

import Restyled.Tracing.App
import Restyled.Tracing.TransactionId

traceAppSegment
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasTracingApp env
       , HasTransactionId env
       )
    => SegmentName
    -> m a
    -> m a
traceAppSegment name = traceSegment (Just name) (Just $ SegmentCategory "App")
