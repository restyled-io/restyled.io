module Restyled.Models.JobLogLine
    ( JobLogLine
    , jobLogLine
    , jobLogLineCreatedAt
    , jobLogLineContent
    ) where

import Restyled.Prelude

import Restyled.Scrub

data JobLogLine = JobLogLine
    { _jobLogLineCreatedAt :: UTCTime
    , _jobLogLineContent :: Text
    }

jobLogLine :: UTCTime -> Text -> JobLogLine
jobLogLine createdAt content = JobLogLine
    { _jobLogLineCreatedAt = createdAt
    , _jobLogLineContent = scrubGitHubToken content
    }

jobLogLineCreatedAt :: JobLogLine -> UTCTime
jobLogLineCreatedAt = _jobLogLineCreatedAt

jobLogLineContent :: JobLogLine -> Text
jobLogLineContent = _jobLogLineContent
