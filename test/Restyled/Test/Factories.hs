module Restyled.Test.Factories
  ( -- * User
    genUser
  , genAdmin

    -- * Job
  , genJob
  , setJobIncomplete
  , setJobComplete

    -- * Marketplace
  , genAccount
  , setAccountUnexpired
  , setAccountExpired
  , setPlanPublic
  , setPlanLimited
  , setPlanUnlimited
  ) where

import Restyled.Prelude hiding (fieldLens)

import qualified Data.List.NonEmpty as NE
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Settings
import Restyled.Test.Graphula
import Restyled.Test.Lens

genUser
  :: GraphulaContext m '[User]
  => Text
  -- ^ Email
  -> (User -> User)
  -> m (Entity User)
genUser email f =
  node @User ()
    $ edit
    $ (fieldLens UserEmail ?~ email)
    . (fieldLens UserCredsIdent .~ email)
    . (fieldLens UserCredsPlugin .~ "dummy")
    . f

genAdmin
  :: ( MonadReader env m
     , HasSettings env
     , MonadTrans t
     , MonadFail (t m)
     , GraphulaContext (t m) '[User]
     )
  => t m (Entity User)
genAdmin = do
  Just emails <- lift $ NE.nonEmpty . appAdmins <$> view settingsL
  genUser (NE.head emails) id

genJob
  :: (MonadIO m, GraphulaContext m '[Job])
  => Entity Repo
  -> (Job -> Job)
  -> m (Entity Job)
genJob (Entity _ Repo {..}) f = do
  pullRequest <- generate arbitrary
  node @Job (repoOwner, repoName, pullRequest) $ edit f

setJobIncomplete :: Job -> Job
setJobIncomplete =
  (fieldLens JobCompletedAt .~ Nothing) . (fieldLens JobExitCode .~ Nothing)

setJobComplete :: UTCTime -> Int -> Job -> Job
setJobComplete at exitCode =
  (fieldLens JobUpdatedAt .~ at)
    . (fieldLens JobCompletedAt ?~ at)
    . (fieldLens JobExitCode ?~ exitCode)

genAccount
  :: GraphulaContext m '[MarketplaceAccount]
  => Entity Repo
  -> Entity MarketplacePlan
  -> (MarketplaceAccount -> MarketplaceAccount)
  -> m (Entity MarketplaceAccount)
genAccount (Entity _ Repo {..}) (Entity planId _) f =
  node @MarketplaceAccount (nameToName repoOwner, planId) $ edit f

setAccountUnexpired :: MarketplaceAccount -> MarketplaceAccount
setAccountUnexpired = fieldLens MarketplaceAccountExpiresAt .~ Nothing

setAccountExpired :: UTCTime -> MarketplaceAccount -> MarketplaceAccount
setAccountExpired t = fieldLens MarketplaceAccountExpiresAt ?~ t

setPlanPublic :: MarketplacePlan -> MarketplacePlan
setPlanPublic =
  fieldLens MarketplacePlanPrivateRepoAllowance .~ PrivateRepoAllowanceNone

setPlanLimited :: Natural -> MarketplacePlan -> MarketplacePlan
setPlanLimited n =
  fieldLens MarketplacePlanPrivateRepoAllowance
    .~ PrivateRepoAllowanceLimited n

setPlanUnlimited :: MarketplacePlan -> MarketplacePlan
setPlanUnlimited =
  fieldLens MarketplacePlanPrivateRepoAllowance
    .~ PrivateRepoAllowanceUnlimited
