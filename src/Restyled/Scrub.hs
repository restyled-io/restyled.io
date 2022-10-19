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
-- This function naively masks @x-access-token:{...}@github.com@, which
-- addresses known error messages. We rely on the ephemeralness of tokens to
-- address unknown messages.
--
scrubGitHubToken :: Text -> Text
scrubGitHubToken msg = fromMaybe msg $ do
    (before, rest) <- breakOnDrop "x-access-token:" msg
    (_, after) <- breakOnDrop "@github.com" rest
    pure $ before <> "x-access-token:***@github.com" <> scrubGitHubToken after

breakOnDrop :: Text -> Text -> Maybe (Text, Text)
breakOnDrop needle = secondM (T.stripPrefix needle) . T.breakOn needle
