module Restyled.Job.Completion
    ( JobCompletion(..)
    , jobCompletion
    ) where

import Restyled.Prelude

import Restyled.Models.DB (Job(..))

data JobCompletion
    = JobSuccess UTCTime
    | JobFailure UTCTime Int
    | JobInProgress

jobCompletion :: Job -> JobCompletion
jobCompletion job = case (jobCompletedAt job, jobExitCode job) of
    (Just completedAt, Just 0) -> JobSuccess completedAt
    (Just completedAt, Just n) -> JobFailure completedAt n
    _ -> JobInProgress
