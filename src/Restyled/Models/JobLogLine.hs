module Restyled.Models.JobLogLine
  ( JobLogLine
  , jobLogLine
  , jobLogLineCreatedAt
  , jobLogLineContent
  , jobLogLineContentJSON
  , jobLogLineContentPatch
  , jobLogLineIsPatch
  ) where

import Restyled.Prelude

import Control.Monad.Logger.Aeson (LoggedMessage (..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Restyled.Scrub

data JobLogLine = JobLogLine
  { _jobLogLineCreatedAt :: UTCTime
  , _jobLogLineContent :: Text
  }

jobLogLine :: UTCTime -> Text -> JobLogLine
jobLogLine createdAt content =
  JobLogLine
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
    $ asum
      [ decode $ BSL.fromStrict $ encodeUtf8 $ jobLogLineContent ll
      , do
          msg <- T.stripPrefix actStarPrefix $ jobLogLineContent ll
          pure
            $ (fakeLoggedMessage ll)
              { loggedMessageLevel = LevelInfo
              , loggedMessageText = "=> " <> msg
              }
      , do
          stripped <- T.stripPrefix actOutputPrefix $ jobLogLineContent ll
          decode $ BSL.fromStrict $ encodeUtf8 stripped
      ]
 where
  actStarPrefix :: Text
  actStarPrefix = "[Agent/restyled] ⭐ Run Main "

  actOutputPrefix :: Text
  actOutputPrefix = "[Agent/restyled]   | "

jobLogLineContentPatch :: JobLogLine -> Maybe Text
jobLogLineContentPatch = loggedMessageTextPatch . jobLogLineContentJSON

jobLogLineIsPatch :: JobLogLine -> Bool
jobLogLineIsPatch = loggedMessageIsPatch . jobLogLineContentJSON

loggedMessageTextPatch :: LoggedMessage -> Maybe Text
loggedMessageTextPatch lm =
  loggedMessageText lm <$ guard (loggedMessageIsPatch lm)

loggedMessageIsPatch :: LoggedMessage -> Bool
loggedMessageIsPatch LoggedMessage {..} = fromMaybe False $ do
  Bool patch <- KeyMap.lookup "patch" loggedMessageThreadContext
  pure patch

fakeLoggedMessage :: JobLogLine -> LoggedMessage
fakeLoggedMessage ll =
  LoggedMessage
    { loggedMessageTimestamp = jobLogLineCreatedAt ll
    , loggedMessageLevel = level
    , loggedMessageLoc = Nothing
    , loggedMessageLogSource = Nothing
    , loggedMessageThreadContext = mempty
    , loggedMessageText = content
    , loggedMessageMeta = mempty
    }
 where
  (content, level) = stripLogLevel $ jobLogLineContent ll

stripLogLevel :: Text -> (Text, LogLevel)
stripLogLevel content =
  maybe (content, LevelOther "trace") getFirst
    $ foldMap (fmap First . firstM (`T.stripPrefix` content)) stylePrefixes

stylePrefixes :: [(Text, LogLevel)]
stylePrefixes =
  [ ("[Debug] ", LevelDebug)
  , ("[Info] ", LevelInfo)
  , ("[Warn] ", LevelWarn)
  , ("[Error] ", LevelError)
  ]
