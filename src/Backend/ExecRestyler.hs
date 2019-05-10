{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Execution of the Restyler process
module Backend.ExecRestyler
    ( ExecRestyler(..)
    , FailedExecRestyler(..)
    , SucceededExecRestyler(..)
    , runExecRestyler
    )
where

import Import

import Backend.AcceptedJob
import Control.Monad.Except
import System.Exit (ExitCode(..))

-- | TODO: try not to need @'Entity'@s
newtype ExecRestyler m = ExecRestyler
    { unExecRestyler :: Entity Repo -> Entity Job -> m (ExitCode, String, String)
    }

newtype FailedExecRestyler = FailedExecRestyler
    { unFailedExecRestyler :: Entity Job
    }

newtype SucceededExecRestyler = SucceededExecRestyler
    { unSucceededExecRestyler :: Entity Job
    }

runExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT FailedExecRestyler m SucceededExecRestyler
runExecRestyler (ExecRestyler execRestyler) AcceptedJob {..} = do
    now <- liftIO getCurrentTime
    fmap (success now)
        $ withExceptT (failure now)
        $ ExceptT
        $ tryAny
        $ execRestyler ajRepo ajJob
  where
    Entity jobId job = ajJob

    success now (ec', out, err) =
        let
            ec = case ec' of
                ExitSuccess -> 0
                ExitFailure c -> c
        in
            SucceededExecRestyler $ Entity
                jobId
                job
                    { jobUpdatedAt = now
                    , jobCompletedAt = Just now
                    , jobExitCode = Just ec
                    , jobStdout = Just $ pack out
                    , jobStderr = Just $ pack err
                    }

    failure now ex = FailedExecRestyler $ Entity
        jobId
        job
            { jobUpdatedAt = now
            , jobCompletedAt = Just now
            , jobExitCode = Just 99
            , jobStdout = Just ""
            , jobStderr = Just $ tshow ex
            }
