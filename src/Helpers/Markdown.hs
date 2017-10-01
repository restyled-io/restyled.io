{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE QuasiQuotes #-}
module Helpers.Markdown
    ( markdownFromTemplate
    , markdownToWidget
    , markdownToHtml
    , textFile
    ) where

import Import

import Language.Haskell.TH.Syntax (Exp, Q)
import Skylighting
import Text.Pandoc
import qualified Data.Text.Internal.Builder as TB
import qualified Text.Shakespeare.Text as ST

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
    . writeHtmlString def { writerHighlight = True }
    . handleError
    . readMarkdown def -- { readerExtensions = githubMarkdownExtensions }
    . unpack

textFile :: FilePath -> Q Exp
textFile = ST.textFile
-- ^ if this works, then why doesn't this work?
--textFile = if development then ST.textFileReload else ST.textFile
