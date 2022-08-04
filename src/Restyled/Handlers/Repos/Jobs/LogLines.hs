module Restyled.Handlers.Repos.Jobs.LogLines
    ( getRepoJobLogLinesR
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Restyled.DB
import Restyled.Foundation
import Restyled.JobOutput
import Restyled.Models
import Restyled.WebSockets
import qualified Restyled.Widgets.Job as Widgets
import Restyled.Yesod
import Text.Blaze.Html.Renderer.Text (renderHtml)

getRepoJobLogLinesR :: OwnerName -> RepoName -> JobId -> Handler Text
getRepoJobLogLinesR _owner _name jobId = do
    job <- runDB $ getEntity404 jobId

    -- Use 28s to be just under the Heroku 30s timeout
    let keepAlivePeriod :: Int
        keepAlivePeriod = 28

    keepAliveMsg <- renderKeepAliveMessage

    webSockets keepAlivePeriod keepAliveMsg $ \send -> do
        conn <- ask
        lift $ followJobOutput jobId $ \jobLogLines -> do
            htmls <- mconcat <$> traverse renderJobLogLine jobLogLines
            runReaderT (void $ send htmls) conn

    -- If not accessed via WebSockets, respond with plain text Job log
    jobLogLines <- fetchJobOutput job
    pure $ T.unlines $ map jobLogLineContent jobLogLines

renderJobLogLine :: JobLogLine -> Handler LT.Text
renderJobLogLine ln = do
    pc <- widgetToPageContent $ Widgets.jobLogLine ln
    renderHtml <$> withUrlRenderer (pageBody pc)

renderKeepAliveMessage :: Handler LT.Text
renderKeepAliveMessage = do
    now <- getCurrentTime
    renderJobLogLine $ jobLogLine
        now
        "No output in the last 30 seconds. Continuing to wait..."
