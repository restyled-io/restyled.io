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
import System.Directory (getCurrentDirectory)
import System.Process (callProcess)

callRestylers :: Branch -> IO (Either String ())
callRestylers mergeBase = runExceptT $ do
    Config{..} <- ExceptT loadConfig

    unless cEnabled $
        throwError "Restyler disabled by config"

    dir <- tryE getCurrentDirectory
    paths <- tryE $ changedPaths mergeBase
    tryE $ for_ cRestylers $ \r@Restyler{..} ->
        callProcess "docker" $ dockerArguments dir r
            ++ rArguments
            ++ restylePaths r paths

dockerArguments :: FilePath -> Restyler -> [String]
dockerArguments dir Restyler{..} =
    [ "run", "--rm"
    , "--volume", dir <> ":/code"
    , "--net", "none"
    , "restyled/restyler-" <> rName
    , rCommand
    ]

tryE :: IO a -> ExceptT String IO a
tryE f = ExceptT $ first show <$> tryIO f
