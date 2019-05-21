-- | Execution of the Restyler process
module Backend.ExecRestyler
    ( ExecRestyler(..)
    , ExecRestylerFailed(..)
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
    { unExecRestyler :: Entity Repo -> Entity Job -> m ExitCode
    }

data ExecRestylerFailed = ExecRestylerFailed
    { erfException :: SomeException
    , erfJob :: Entity Job
    }

runExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT ExecRestylerFailed m (Entity Job)
runExecRestyler execRestyler aj =
    bimapMExceptT (failure $ ajJob aj) (success $ ajJob aj)
        $ tryExecRestyler execRestyler aj

success :: MonadIO m => Entity Job -> ExitCode -> m (Entity Job)
success job ec = do
    now <- liftIO getCurrentTime
    pure $ overEntity job $ completeJob now ec

failure :: MonadIO m => Entity Job -> SomeException -> m ExecRestylerFailed
failure job ex = do
    now <- liftIO getCurrentTime
    pure $ ExecRestylerFailed ex $ overEntity job $ completeJobErroredS now ex

-- | Run the @'ExecRestyler'@ and capture exceptions to @'ExceptT'@
tryExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT SomeException m ExitCode
tryExecRestyler (ExecRestyler execRestyler) AcceptedJob {..} =
    ExceptT $ tryAny $ execRestyler ajRepo ajJob
