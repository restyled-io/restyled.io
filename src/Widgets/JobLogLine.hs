module Widgets.JobLogLine
    ( renderJobLogLine
    )
where

import Import

import qualified Data.Text.Lazy as LT
import Foundation
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Widgets.Job
import Yesod

renderJobLogLine :: Entity JobLogLine -> Handler LT.Text
renderJobLogLine logLine = do
    pc <- widgetToPageContent $ colorizedJobLogLine logLine
    renderHtml <$> withUrlRenderer (pageBody pc)
