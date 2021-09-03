{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Restyled.Widgets.Job
    ( jobCard
    , jobOutput

    -- * Job completion
    , Completion(..)
    , jobCompletion

    -- * Utilities
    , textJobLogLine
    , colorizedJobLogLine
    , scrubGitHubToken
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import Formatting (format)
import Formatting.Time (diff)
import qualified Network.URI.Encode as URI
import Restyled.Foundation
import Restyled.Models
import Restyled.Routes
import Restyled.Settings
import Restyled.Widgets.ContainsURLs
import Restyled.Yesod
import Text.Julius (RawJS(..))
import Text.Shakespeare.Text (st)

-- | Internal helper for rendering completion state
data Completion
    = Success UTCTime
    | Failure UTCTime Int
    | InProgress

jobCompletion :: Job -> Completion
jobCompletion job = case (jobCompletedAt job, jobExitCode job) of
    (Just completedAt, Just 0) -> Success completedAt
    (Just completedAt, Just n) -> Failure completedAt n
    _ -> InProgress

jobCard :: Entity Job -> Widget
jobCard jobE@(Entity jobId job) = do
    now <- liftIO getCurrentTime
    issueUrl <- jobIssueURL jobE <$> getUrlRender
    $(widgetFile "widgets/job-card")

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

textJobLogLine :: JobLogLine -> Text
textJobLogLine = scrubGitHubToken . jobLogLineContent

colorizedJobLogLine :: JobLogLine -> Widget
colorizedJobLogLine = colorizedLogLine . scrubGitHubToken . jobLogLineContent

colorizedLogLine :: Text -> Widget
colorizedLogLine ln
    | Just message <- T.stripPrefix "[Debug] " ln = logLine "debug" message
    | Just message <- T.stripPrefix "[Info] " ln = logLine "info" message
    | Just message <- T.stripPrefix "[Warn] " ln = logLine "warn" message
    | Just message <- T.stripPrefix "[Error] " ln = logLine "error" message
    | otherwise = logLine "unknown" ln
  where
    logLine :: Text -> Text -> Widget
    logLine level message = $(widgetFile "widgets/log-line")

-- | /Naively/ scrub ephemeral tokens from log messages
--
-- If there's an error cloning or pushing, it may show the remote's URL which
-- will include the "x-access-token:...@github.com" secret. These are ephemeral
-- and only valid for less than 5 minutes, but we shouldn't show them anyway.
--
-- This function naively strips the 58 characters before any appearance of
-- "@github.com", which addresses known error messages and should fail-safe by
-- over-scrubbing when it gets something wrong.
--
scrubGitHubToken :: Text -> Text
scrubGitHubToken msg = fromMaybe msg $ do
    (before, after) <- breakOnDrop "@github.com" msg
    pure
        $ T.dropEnd 58 before
        <> "<SCRUBBED>@github.com"
        <> scrubGitHubToken after

breakOnDrop :: Text -> Text -> Maybe (Text, Text)
breakOnDrop needle = bimapM pure (T.stripPrefix needle) . T.breakOn needle
