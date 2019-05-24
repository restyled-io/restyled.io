module Restyled.Handlers.CommonSpec
    ( spec
    )
where

import Restyled.Test

spec :: Spec
spec = withApp $ do
    describe "robots.txt" $ do
        it "gives a 200" $ do
            get RobotsR
            statusIs 200
        it "has correct User-agent" $ do
            get RobotsR
            bodyContains "User-agent: *"
    describe "favicon.ico" $ do
        it "gives a 200" $ do
            get FaviconR
            statusIs 200

    describe "/revision" $ do
        it "gives a 200" $ do
            get RevisionR
            statusIs 200
