module Restyled.Widgets.JobSpec
    ( spec
    )
where

import Restyled.Test

import Restyled.Widgets.Job (scrubGitHubToken)

spec :: Spec
spec = do
    describe "scrubGitHubToken" $ do
        it "scrubs GitHub tokens" $ do
            let logLines :: [Text]
                logLines =
                    [ "Branch 'if-non-unique-operator-channel-id' set up to track remote branch 'if-non-unique-operator-...skipping..."
                    , "error: failed to push some refs to 'https://x-access-token:v1.aaaaaaaaaabbbbbbbbbbbbbbbcccccccccdddddd@github.com/Foo/bar.git'"
                    , " ! [rejected]        refactor-modules-restyled -> refactor-modules-restyled (stale info)"
                    , "To https://github.com/Foo/bar.git"
                    ]

                scrubbed = map scrubGitHubToken logLines

            shouldBe @IO
                scrubbed
                [ "Branch 'if-non-unique-operator-channel-id' set up to track remote branch 'if-non-unique-operator-...skipping..."
                , "error: failed to push some refs to 'https://<SCRUBBED>@github.com/Foo/bar.git'"
                , " ! [rejected]        refactor-modules-restyled -> refactor-modules-restyled (stale info)"
                , "To https://github.com/Foo/bar.git"
                ]
