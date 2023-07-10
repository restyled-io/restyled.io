module Restyled.Handlers.Repos.Pulls
  ( getRepoPullR
  ) where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Handlers.Repos.Pulls.Jobs (getRepoPullJobsR)
import Restyled.Yesod

getRepoPullR :: OwnerName -> RepoName -> PullRequestNum -> Handler Html
getRepoPullR = getRepoPullJobsR
