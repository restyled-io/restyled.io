{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Restyled.Widgets.Job
    ( jobCard
    , jobListItem
    , jobOutput
    , jobLogLine
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import Formatting (format)
import Formatting.Time (diff)
import qualified Network.URI.Encode as URI
import Restyled.Foundation
import Restyled.Job.Completion
import Restyled.Models hiding (jobLogLine)
import Restyled.Routes
import Restyled.Settings
import Restyled.Widgets.ContainsURLs
import Restyled.Yesod
import Text.Julius (RawJS(..))
import Text.Shakespeare.Text (st)

jobCard :: Entity Job -> Widget
jobCard jobE@(Entity jobId job) = do
    now <- liftIO getCurrentTime
    issueUrl <- jobIssueURL jobE <$> getUrlRender
    $(widgetFile "widgets/job-card")

jobListItem :: Entity Job -> Widget
jobListItem (Entity jobId job) = do
    now <- liftIO getCurrentTime
    $(widgetFile "widgets/job-list-item")

jobIssueURL :: Entity Job -> (Route App -> Text) -> Text
jobIssueURL (Entity jobId Job {..}) urlRender =
    "https://github.com/restyled-io/restyler/issues/new"
        <> "?title="
        <> URI.encodeText title
        <> "&body="
        <> URI.encodeText body
  where
    title = "Problem with Restyler Job #" <> toPathPiece jobId
    body = [st|
Please use the below template to report an issue with this Job. **Don't submit
sensitive information**, this is a public project. If you'd rather communicate
privately, email support@restyled.io.

---

Hi there-

I'm having a problem with a Restyled Job

- **Restyled Job**: #{urlRender $ repoP jobOwner jobRepo $ jobR jobId}
- **GitHub PR**: #{repoPullPath jobOwner jobRepo jobPullRequest}

**What I expected to happen**:

**What happened instead**:

**Configuration** (if applicable):

```yaml
# .restyled.yaml as it was in the commit for the Job
```
|]

jobOutput :: Entity Job -> Widget
jobOutput (Entity jobId job) = $(widgetFile "widgets/job-output")
    where streamElementId = "logs-job-id-" <> toPathPiece jobId

jobLogLine :: JobLogLine -> Widget
jobLogLine ln = $(widgetFile "widgets/log-line")
    where (content, levelStyle) = getLevelStyle $ jobLogLineContent ln

data LevelStyle
    = LevelStyleUnknown
    | LevelStyleDebug
    | LevelStyleInfo
    | LevelStyleWarn
    | LevelStyleError

levelStyleClass :: LevelStyle -> Text
levelStyleClass = \case
    LevelStyleUnknown -> "level-unknown"
    LevelStyleDebug -> "level-debug"
    LevelStyleInfo -> "level-info"
    LevelStyleWarn -> "level-warn"
    LevelStyleError -> "level-error"

levelStyleContent :: LevelStyle -> Text
levelStyleContent = \case
    LevelStyleUnknown -> "unknown"
    LevelStyleDebug -> "debug"
    LevelStyleInfo -> "info"
    LevelStyleWarn -> "warn"
    LevelStyleError -> "error"

getLevelStyle :: Text -> (Text, LevelStyle)
getLevelStyle content = maybe (content, LevelStyleUnknown) getFirst
    $ foldMap (fmap First . firstM (`T.stripPrefix` content)) stylePrefixes

stylePrefixes :: [(Text, LevelStyle)]
stylePrefixes =
    [ ("[Debug] ", LevelStyleDebug)
    , ("[Info] ", LevelStyleInfo)
    , ("[Warn] ", LevelStyleWarn)
    , ("[Error] ", LevelStyleError)
    ]
