-- | Execution of the Restyler process
module Backend.ExecRestyler
    ( ExecRestyler(..)
    , FailedExecRestyler(..)
    , SucceededExecRestyler(..)
    , runExecRestyler
    , tryExecRestyler
    )
where

import Backend.Import

import Backend.AcceptedJob

-- | Execution of the Restyler process
--
-- TODO: try not to need @'Entity'@s, try not to need @'Repo'@.
--
newtype ExecRestyler m = ExecRestyler
    { unExecRestyler :: Entity Repo -> Entity Job -> m (ExitCode, String, String)
    }

-- | A @'Job'@ already updated from a failed execution
newtype FailedExecRestyler = FailedExecRestyler
    { unFailedExecRestyler :: Entity Job
    }

-- | A @'Job'@ already updated from a successful execution
newtype SucceededExecRestyler = SucceededExecRestyler
    { unSucceededExecRestyler :: Entity Job
    }

-- | Like @'tryExecRestyler'@ but wrapping the cases in above newtypes
runExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT FailedExecRestyler m SucceededExecRestyler
runExecRestyler execRestyler aj = do
    now <- liftIO getCurrentTime
    bimapExceptT (failure now) (success now) $ tryExecRestyler execRestyler aj
  where
    job = ajJob aj
    success now = SucceededExecRestyler . overEntity job . completeJob now
    failure now = FailedExecRestyler . overEntity job . completeJobErroredS now

-- | Run the @'ExecRestyler'@ and capture exceptions to @'ExceptT'@
tryExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT SomeException m (ExitCode, String, String)
tryExecRestyler (ExecRestyler execRestyler) AcceptedJob {..} =
    ExceptT $ tryAny $ execRestyler ajRepo ajJob
