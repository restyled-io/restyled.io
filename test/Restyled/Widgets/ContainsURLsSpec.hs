module Restyled.Widgets.ContainsURLsSpec
    ( spec
    )
where

import Restyled.Test

import Restyled.Widgets.ContainsURLs
import Text.Megaparsec (errorBundlePretty, parse)

spec :: Spec
spec = do
    describe "containsURLs" $ do
        it "round-trips" . property $ \(Parsed c) ->
            containsURLs (concatParts c) == c

        it "parses URLs within content" $ do
            "Hi http://google.com there."
                `shouldParseTo` [ ContentPart "Hi "
                                , URLPart "http://google.com"
                                , ContentPart " there."
                                ]

concatParts :: ContainsURLs -> Text
concatParts (ContainsURLs parts) = mconcat $ map toText parts
  where
    toText (ContentPart t) = t
    toText (URLPart t) = t

shouldParseTo :: Text -> [ContentPart] -> Expectation
shouldParseTo input expected =
    either (expectationFailure . errorBundlePretty) (`shouldBe` expected)
        $ parse contentPartsP "" input

newtype Parsed = Parsed ContainsURLs
    deriving Show

instance Arbitrary Parsed where
    arbitrary = do
        n <- getPositive <$> arbitrary

        Parsed . ContainsURLs . spaced . take n <$> oneof
            [ alternating contentPart urlPart
            , alternating urlPart contentPart
            ]

-- | Add a leading space to any @'ContentPart'@ after a @'URLPart'@
--
-- URL parts are (naively) parsed by ending whitespace, so any Content parts
-- parsed after a URL part would always have that.
--
spaced :: [ContentPart] -> [ContentPart]
spaced (URLPart url : ContentPart t : rest) =
    URLPart url : ContentPart (" " <> t) : spaced rest
spaced [] = []
spaced (x : xs) = x : spaced xs

contentPart :: Gen ContentPart
contentPart = ContentPart . pack . getNonEmpty <$> arbitrary

urlPart :: Gen ContentPart
urlPart = URLPart <$> url
  where
    url = (<>) <$> scheme <*> withoutSpaces
    scheme = elements ["http://", "https://"]

withoutSpaces :: Gen Text
withoutSpaces = do
    n <- arbitrary `suchThat` (> 0)
    pack <$> replicateM n nonSpace

nonSpace :: Gen Char
nonSpace = arbitrary `suchThat` (not . isSpace)

-- | Infinite list of alternating values from the two given
alternating :: Applicative f => f a -> f a -> f [a]
alternating a b = (:) <$> a <*> alternating b a
