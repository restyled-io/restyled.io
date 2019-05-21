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
runExecRestyler execRestyler aj =
    bimapMExceptT (failure $ ajJob aj) (success $ ajJob aj)
        $ tryExecRestyler execRestyler aj

success
    :: MonadIO m
    => Entity Job
    -> (ExitCode, String, String)
    -> m SucceededExecRestyler
success job ec = do
    now <- liftIO getCurrentTime
    pure $ SucceededExecRestyler $ overEntity job $ completeJob now ec

failure :: MonadIO m => Entity Job -> SomeException -> m FailedExecRestyler
failure job ex = do
    now <- liftIO getCurrentTime
    pure $ FailedExecRestyler $ overEntity job $ completeJobErroredS now ex

-- | Run the @'ExecRestyler'@ and capture exceptions to @'ExceptT'@
tryExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT SomeException m (ExitCode, String, String)
tryExecRestyler (ExecRestyler execRestyler) AcceptedJob {..} =
    ExceptT $ tryAny $ execRestyler ajRepo ajJob
