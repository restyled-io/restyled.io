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

-- | TODO: try not to need @'Entity'@s
newtype ExecRestyler m = ExecRestyler
    { unExecRestyler :: Entity Repo -> Entity Job -> m (ExitCode, String, String)
    }

newtype FailedExecRestyler = FailedExecRestyler
    { unFailedExecRestyler :: Entity Job
    }

newtype SucceededExecRestyler = SucceededExecRestyler
    { unSucceededExecRestyler :: Entity Job
    }

runExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT FailedExecRestyler m SucceededExecRestyler
runExecRestyler execRestyler aj = do
    now <- liftIO getCurrentTime
    fmap (success now) $ withExceptT (failure now) $ tryExecRestyler
        execRestyler
        aj
  where
    job = ajJob aj
    success now = SucceededExecRestyler . overEntity job . completeJob now
    failure now = FailedExecRestyler . overEntity job . completeJobErrored now

tryExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> ExceptT SomeException m (ExitCode, String, String)
tryExecRestyler (ExecRestyler execRestyler) AcceptedJob {..} =
    ExceptT $ tryAny $ execRestyler ajRepo ajJob
