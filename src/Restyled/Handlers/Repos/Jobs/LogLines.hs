module Restyled.Handlers.Repos.Jobs.LogLines
    ( getRepoJobLogLinesR
    ) where

import Restyled.Prelude

import Conduit
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Restyled.Foundation
import Restyled.JobOutput
import Restyled.Models
import Restyled.WebSockets
import Restyled.Widgets.Job
import Restyled.Widgets.JobLogLine
import Restyled.Yesod

getRepoJobLogLinesR :: OwnerName -> RepoName -> JobId -> Handler Text
getRepoJobLogLinesR _owner _name jobId = do
    job <- runDB $ getEntity404 jobId

    webSockets $ ignoringConnectionException $ do
        conn <- ask

        let conduit :: ConduitT [JobLogLine] [JobLogLine] Handler ()
            conduit = mapMC $ \jobLogLines -> do
                t <- formatJobLogLines jobLogLines
                jobLogLines <$ runReaderT (sendTextDataAck t) conn

        lift $ followJobOutput job conduit

    -- If not accessed via WebSockets, respond with plain text Job log
    jobLogLines <- fetchJobOutput job
    pure $ T.unlines $ map textJobLogLine jobLogLines

formatJobLogLines :: [JobLogLine] -> Handler Text
formatJobLogLines jobLogLines = do
    htmls <- traverse renderJobLogLine jobLogLines
    pure $ TL.toStrict $ mconcat htmls
