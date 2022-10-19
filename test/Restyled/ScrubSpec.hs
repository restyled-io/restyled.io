module Restyled.ScrubSpec
    ( spec
    ) where

import Restyled.Test

import qualified Data.Text as T
import Restyled.Scrub

spec :: Spec
spec = do
    describe "scrubGitHubToken" $ do
        it "scrubs GitHub tokens" $ example $ do
            let logLines :: Text
                logLines = T.unlines
                    [ "Branch 'if-non-unique-operator-channel-id' set up to track remote branch 'if-non-unique-operator-...skipping..."
                    , "error: failed to push some refs to 'https://x-access-token:v1.aaaaaaaaaabbbbbbbbbbbbbbbcccccccccdddddd@github.com/Foo/bar.git'"
                    , "error: failed to push some refs to 'https://x-access-token:ghp_abc23089vbavh348985@github.com/Foo/bar.git'"
                    , " ! [rejected]        refactor-modules-restyled -> refactor-modules-restyled (stale info)"
                    , "To https://github.com/Foo/bar.git"
                    ]

            scrubGitHubToken logLines `shouldBe` T.unlines
                [ "Branch 'if-non-unique-operator-channel-id' set up to track remote branch 'if-non-unique-operator-...skipping..."
                , "error: failed to push some refs to 'https://x-access-token:***@github.com/Foo/bar.git'"
                , "error: failed to push some refs to 'https://x-access-token:***@github.com/Foo/bar.git'"
                , " ! [rejected]        refactor-modules-restyled -> refactor-modules-restyled (stale info)"
                , "To https://github.com/Foo/bar.git"
                ]
