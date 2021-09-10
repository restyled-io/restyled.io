{-# LANGUAGE QuasiQuotes #-}

module Restyled.Handlers.JobsSpec
    ( spec
    ) where

import Restyled.Test

import Control.Lens ((^?!))
import Data.Aeson.Lens
import Data.Aeson.QQ
import qualified Data.Text as T
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey)
import Restyled.Test.Graphula

spec :: Spec
spec = withApp $ do
    describe "PATCH /jobs/:id" $ do
        it "404s for unknown Job" $ graph $ do
            admin <- genAdmin
            lift $ do
                authenticateAs admin

                patchJSON (JobsP $ JobP (toSqlKey 99) JobR) [aesonQQ|{}|]

                statusIs 404

        it "expects a nonempty list of updates" $ graph $ do
            admin <- genAdmin
            repo <- node @Repo () mempty
            job <- genJob repo setJobIncomplete
            lift $ do
                authenticateAs admin

                patchJSON (JobsP $ JobP (entityKey job) JobR) [aesonQQ|[]|]

                statusIs 400
                resp <- getJsonBody
                resp
                    ^?! key "errors"
                    . nth 0
                    . _String
                    `shouldSatisfy` ("parsing NonEmpty failed" `T.isPrefixOf`)

        it "accepts updates and parrots back" $ graph $ do
            admin <- genAdmin
            now <- liftIO getCurrentTime
            repo <- node @Repo () mempty
            job <- genJob repo setJobIncomplete
            lift $ do
                authenticateAs admin

                patchJSON
                    (JobsP $ JobP (entityKey job) JobR)
                    [aesonQQ|
                    [ { "tag": "Complete"
                      , "contents":
                        { "completedAt": #{now}
                        , "exitCode": 99
                        }
                      }
                    ]
                |]

                statusIs 200
                resp <- getJsonBody
                resp ^?! key "id" . _JSON `shouldBe` entityKey job

                Just Job {..} <- runDB $ P.get $ entityKey job
                jobCompletedAt `shouldSatisfy` isJust
                jobExitCode `shouldBe` Just 99
