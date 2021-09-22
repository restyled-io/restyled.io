module Restyled.Models.Job
    (
    -- * Temporary log-transition helpers
      markJobAsCloudWatch
    , jobIsCloudWatch
    ) where

import Restyled.Prelude

import Restyled.Models.DB

markJobAsCloudWatch :: Job -> Job
markJobAsCloudWatch job = job { jobStdout = Just cwSigil }

jobIsCloudWatch :: Job -> Bool
jobIsCloudWatch = (== Just cwSigil) . jobStdout

cwSigil :: Text
cwSigil = "__cw"
