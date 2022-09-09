module Restyled.Handlers.Repos.Jobs.Patch
    ( getRepoJobPatchR
    ) where

import Restyled.Prelude

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
    let patchLines = mapMaybe jobLogLineContentPatch jobLogLines

    if null patchLines
        then sendResponseStatus status404 emptyPatch
        else pure $ T.unlines patchLines
  where
    emptyPatch :: Text
    emptyPatch
        = "Patch not found. There may have been no style differences, or this Job's log may have expired."
