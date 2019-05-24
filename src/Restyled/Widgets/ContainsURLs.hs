{-# LANGUAGE QuasiQuotes #-}

module Restyled.Widgets.ContainsURLs
    ( renderWithURLs

    -- * Exported to be tested
    , ContainsURLs(..)
    , containsURLs
    , ContentPart(..)
    , contentPartsP
    )
where

import Restyled.Prelude hiding (some, try)

import Restyled.Foundation
import Restyled.Yesod
import Text.Megaparsec
import Text.Megaparsec.Char

data ContentPart
    = ContentPart Text
    | URLPart Text
    deriving (Eq, Show)

newtype ContainsURLs = ContainsURLs [ContentPart]
    deriving (Eq, Show)

renderWithURLs :: Text -> Widget
renderWithURLs = renderContainsURLs . containsURLs

renderContainsURLs :: ContainsURLs -> Widget
renderContainsURLs (ContainsURLs parts) = mconcat $ map renderContentPart parts

-- brittany-disable-next-binding

renderContentPart :: ContentPart -> Widget
renderContentPart (ContentPart t) = [whamlet|#{t}|]
renderContentPart (URLPart url) = [whamlet|
    $newline never
    <a href=#{url}>#{url}
|]

containsURLs :: Text -> ContainsURLs
containsURLs t = ContainsURLs $ fromMaybe [ContentPart t] $ contentParts t

contentParts :: Text -> Maybe [ContentPart]
contentParts = hush . parse contentPartsP ""

type Parser = Parsec Void Text

contentPartsP :: Parser [ContentPart]
contentPartsP = manyTill (urlP <|> contentP) eof

urlP :: Parser ContentPart
urlP = URLPart <$> url
    where url = (<>) <$> schemeP <*> (pack <$> some (satisfy $ not . isSpace))

contentP :: Parser ContentPart
contentP =
    ContentPart . pack <$> manyTill anySingle (void (lookAhead urlP) <|> eof)

schemeP :: Parser Text
schemeP = string "https://" <|> string "http://"
