module Restyled.Handlers.Repos
    ( getRepoR
    , putRepoR
    ) where

import Restyled.Prelude

import Control.Monad.Validate
import Restyled.Api.UpsertRepo
import Restyled.Foundation
import Restyled.Handlers.Repos.Jobs
import Restyled.Yesod

getRepoR :: OwnerName -> RepoName -> Handler Html
getRepoR = getRepoJobsR

-- | Find or create a repository
--
-- - Request: 'ApiUpsertRepo'
-- - Response: 400 with 'ApiUpsertRepoErrors' or 200 with 'ApiRepo'
--
putRepoR :: OwnerName -> RepoName -> Handler Value
putRepoR owner name = do
    debug "start"
    body <- requireJsonBody
    debug $ tshow body
    result <- runDB $ runValidateT $ do
        lift $ lift $ debug "Pre-validate"
        assertOwnerName owner body *> assertRepoName name body
        lift $ lift $ debug "Post-validate; pre-upsert"
        upsertRepo body
    debug $ "Post-upsert: " <> tshow result
    either (sendStatusJSON status400) (sendStatusJSON status200) result

debug :: MonadLogger m => Text -> m ()
debug msg = logInfoN $ "[putRepoR]: " <> msg
