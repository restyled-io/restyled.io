{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
-- |
--
-- N.B. Module extracted primarily for testability.
--
module Restyler.Run
    ( callRestylers
    ) where

import ClassyPrelude

import Control.Monad.Except
import Data.Bifunctor (first)
import GitHub.Model (Branch)
import Restyler.Clone
import Restyler.Config
import System.Process (callProcess)

callRestylers :: Branch -> IO (Either String ())
callRestylers mergeBase = runExceptT $ do
    Config{..} <- ExceptT loadConfig

    unless cEnabled $
        throwError "Restyler disabled by config"

    paths <- tryE $ changedPaths mergeBase
    tryE $ for_ cRestylers $ \r@Restyler{..} ->
        callProcess rCommand $ rArguments ++ restylePaths r paths

tryE :: IO a -> ExceptT String IO a
tryE f = ExceptT $ first show <$> tryIO f
