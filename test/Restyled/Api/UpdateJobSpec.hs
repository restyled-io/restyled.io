{-# LANGUAGE QuasiQuotes #-}

module Restyled.Api.UpdateJobSpec
  ( spec
  ) where

import Restyled.Test

import Data.Aeson.QQ
import qualified Database.Persist as P
import Database.Persist.Sql (toSqlKey)
import Restyled.Api.UpdateJob
  ( ApiCompleteJob (ApiCompleteJob)
  , ApiUpdateJob (Complete)
  , updateJob
  )
import qualified Restyled.Api.UpdateJob as ApiUpdateJob
import Restyled.Test.Graphula

spec :: Spec
spec = do
  describe "ApiUpdateJob" $ do
    let
      expectDecode :: (HasCallStack, MonadIO m) => Value -> m ()
      expectDecode =
        void
          . expectRight "ApiUpdateJob"
          . eitherDecode @ApiUpdateJob
          . encode

    it "decodes" $ example $ do
      now <- getCurrentTime
      expectDecode
        [aesonQQ|
                { "tag": "Complete"
                , "contents":
                    { "completedAt": #{now}
                    , "exitCode": 0
                    }
                }
            |]

  describe "ApiUpdateJobErrors" $ do
    it "encodes" $ example $ do
      let errs = ApiUpdateJob.updatedJobNotFound $ toSqlKey 99

      toJSON errs
        `shouldMatchJson` [aesonQQ|
                { "errors":
                    [ { "tag": "UpdatedJobNotFound"
                      , "contents": 99
                      }
                    ]
                }
            |]

  withApp $ do
    describe "updateJob" $ do
      it "sets values given" $ graph $ do
        now <- liftIO getCurrentTime
        repo <- node @Repo () mempty
        job <- genJob repo setJobIncomplete

        lift $ runDB $ do
          void
            $ assertValidateT
            $ updateJob (entityKey job)
            $ pure
            $ Complete
            $ ApiCompleteJob
              { ApiUpdateJob.completedAt = now
              , ApiUpdateJob.exitCode = 99
              }

          Just Job {..} <- P.get $ entityKey job

          jobCompletedAt `shouldSatisfy` isJust
          jobExitCode `shouldBe` Just 99
