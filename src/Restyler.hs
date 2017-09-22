{-# LANGUAGE NoImplicitPrelude #-}
module Restyler
    ( runRestyler
    ) where

import Import

runRestyler :: MonadIO m => Entity PullRequest -> m ()
runRestyler _ = return ()
