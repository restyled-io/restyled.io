module Restyled.Handlers.Repos.Jobs.LogLines
    ( getRepoJobLogLinesR
    ) where

import Restyled.Prelude

import Conduit
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Last(..))
import qualified Data.Text as T
import qualified Network.WebSockets as WebSockets
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

        lift
            $ followJobOutput job
            $ mapMC (sendLogLines conn)
            .| foldMapC getLastCreatedAt

    -- If not accessed via WebSockets, respond with plain text Job log
    jobLogLines <- fetchJobOutput job
    pure $ T.unlines $ map textJobLogLine jobLogLines

sendLogLines :: WebSockets.Connection -> [JobLogLine] -> Handler [JobLogLine]
sendLogLines conn jobLogLines = do
    htmls <- traverse renderJobLogLine jobLogLines
    jobLogLines <$ runReaderT (sendTextDataAck $ mconcat htmls) conn

getLastCreatedAt :: [JobLogLine] -> Maybe (Last UTCTime)
getLastCreatedAt = fmap (Last . jobLogLineCreatedAt . NE.last) . NE.nonEmpty
