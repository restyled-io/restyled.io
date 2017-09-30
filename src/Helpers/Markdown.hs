{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Helpers.Markdown
    ( markdownFromTemplate
    , markdownToWidget
    , markdownToHtml
    , textFile
    ) where

import Import

import Skylighting
import Text.Pandoc
import Text.Shakespeare.Text (textFile)
import qualified Data.Text.Internal.Builder as TB

markdownFromTemplate :: (render -> TB.Builder) -> render -> Widget
markdownFromTemplate t r = markdownToWidget $ toStrict $ TB.toLazyText $ t r

markdownToWidget :: Text -> Widget
markdownToWidget t = do
    let css = preEscapedToMarkup $ styleToCss zenburn
        html = preEscapedToMarkup $ markdownToHtml t

    toWidgetHead $ [hamlet|<style>#{css}|]
    toWidget $ [hamlet|#{html}|]


markdownToHtml :: Text -> Text
markdownToHtml = pack
    . writeHtmlString def
        { writerHighlight = True
        --, writerSectionDivs = True
        }
    . transformPandoc
    . handleError
    . readMarkdown def
    . unpack

transformPandoc :: Pandoc -> Pandoc
transformPandoc p = traceShowId p
