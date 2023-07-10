module Restyled.Test.Validate
  ( module Control.Monad.Validate
  , assertValidateT
  ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Test.Expectations (expectationFailure)

assertValidateT :: (MonadIO m, Show e) => ValidateT e m a -> m a
assertValidateT = either (expectationFailure . message) pure <=< runValidateT
 where
  message :: Show e => e -> String
  message = ("Expected validation to succeed, received: " <>) . show
