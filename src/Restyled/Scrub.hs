module Restyled.Scrub
    ( scrubGitHubToken
    ) where

import Restyled.Prelude

import qualified Data.Text as T

-- | /Naively/ scrub ephemeral tokens from log messages
--
-- If there's an error cloning or pushing, it may show the remote's URL which
-- will include the "x-access-token:...@github.com" secret. These are ephemeral
-- and only valid for less than 5 minutes, but we shouldn't show them anyway.
--
-- This function naively strips the 58 characters before any appearance of
-- "@github.com", which addresses known error messages and should fail-safe by
-- over-scrubbing when it gets something wrong.
--
scrubGitHubToken :: Text -> Text
scrubGitHubToken msg = fromMaybe msg $ do
    (before, after) <- breakOnDrop "@github.com" msg
    pure
        $ T.dropEnd 58 before
        <> "<SCRUBBED>@github.com"
        <> scrubGitHubToken after

breakOnDrop :: Text -> Text -> Maybe (Text, Text)
breakOnDrop needle = secondM (T.stripPrefix needle) . T.breakOn needle
