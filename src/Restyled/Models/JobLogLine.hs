module Restyled.Models.JobLogLine
    ( JobLogLine
    , jobLogLine
    , jobLogLineCreatedAt
    , jobLogLineContent
    , jobLogLineContentJSON
    ) where

import Restyled.Prelude

import Control.Monad.Logger.Aeson (LoggedMessage(..))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
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

jobLogLineContentJSON :: JobLogLine -> LoggedMessage
jobLogLineContentJSON ll =
    fromMaybe (fakeLoggedMessage ll)
        $ decode
        $ BSL.fromStrict
        $ encodeUtf8
        $ jobLogLineContent ll

fakeLoggedMessage :: JobLogLine -> LoggedMessage
fakeLoggedMessage ll = LoggedMessage
    { loggedMessageTimestamp = jobLogLineCreatedAt ll
    , loggedMessageLevel = level
    , loggedMessageLoc = Nothing
    , loggedMessageLogSource = Nothing
    , loggedMessageThreadContext = mempty
    , loggedMessageText = content
    , loggedMessageMeta = mempty
    }
    where (content, level) = stripLogLevel $ jobLogLineContent ll

stripLogLevel :: Text -> (Text, LogLevel)
stripLogLevel content = maybe (content, LevelInfo) getFirst
    $ foldMap (fmap First . firstM (`T.stripPrefix` content)) stylePrefixes

stylePrefixes :: [(Text, LogLevel)]
stylePrefixes =
    [ ("[Debug] ", LevelDebug)
    , ("[Info] ", LevelInfo)
    , ("[Warn] ", LevelWarn)
    , ("[Error] ", LevelError)
    ]
