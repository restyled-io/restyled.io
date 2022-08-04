module Restyled.Handlers.Repos.Jobs.Patch
    ( getRepoJobPatchR
    ) where

import Restyled.Prelude

import Control.Monad.Logger.Aeson (LoggedMessage(..))
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import Restyled.DB
import Restyled.Foundation
import Restyled.JobOutput
import Restyled.Models
import Restyled.Yesod

getRepoJobPatchR :: OwnerName -> RepoName -> JobId -> Handler Text
getRepoJobPatchR _owner _name jobId = do
    job <- runDB $ getEntity404 jobId
    jobLogLines <- fetchJobOutput job
    let patchLines = mapMaybe toPatchLine jobLogLines

    if null patchLines
        then sendResponseStatus status404 emptyPatch
        else pure $ T.unlines patchLines
  where
    emptyPatch :: Text
    emptyPatch
        = "Patch not found. There may have been no style differences, or this Job's log may have expired."

toPatchLine :: JobLogLine -> Maybe Text
toPatchLine ll = do
    Bool patch <- KeyMap.lookup "patch" loggedMessageThreadContext
    loggedMessageText <$ guard patch
    where LoggedMessage {..} = jobLogLineContentJSON ll
