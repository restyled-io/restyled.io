module Restyled.Handlers.Repos.Jobs.LogLines
    ( getRepoJobLogLinesR
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import Restyled.Foundation
import Restyled.JobOutput
import Restyled.Models
import Restyled.WebSockets
import Restyled.Widgets.Job
import Restyled.Widgets.JobLogLine
import Restyled.Yesod

getRepoJobLogLinesR :: OwnerName -> RepoName -> JobId -> Handler Text
getRepoJobLogLinesR _owner _name jobId = do
    void $ runDB $ get404 jobId

    webSockets $ ignoringConnectionException $ do
        conn <- ask

        lift $ followJobOutput jobId $ \jobLogLines -> do
            htmls <- mconcat <$> traverse renderJobLogLine jobLogLines
            runReaderT (void $ sendTextDataAck htmls) conn

    -- If not accessed via WebSockets, respond with plain text Job log
    jobLogLines <- fetchJobOutput jobId
    pure $ T.unlines $ map textJobLogLine jobLogLines
