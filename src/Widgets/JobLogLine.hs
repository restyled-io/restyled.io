module Widgets.JobLogLine
    ( renderJobLogLine
    )
where

import Import

import qualified Data.Text.Lazy as LT
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Widgets.Job

renderJobLogLine :: Entity JobLogLine -> Handler LT.Text
renderJobLogLine (Entity _ JobLogLine {..}) = do
    pc <- widgetToPageContent
        $ colorizedLogLine jobLogLineStream jobLogLineContent
    renderHtml <$> withUrlRenderer (pageBody pc)
