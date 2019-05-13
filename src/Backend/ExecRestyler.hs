-- | Execution of the Restyler process
module Backend.ExecRestyler
    ( ExecRestyler(..)
    , FailedExecRestyler(..)
    , SucceededExecRestyler(..)
    , runExecRestyler
    )
where

import Import.NoFoundation

import Backend.AcceptedJob
import Control.Monad.Except
import System.Exit (ExitCode(..))

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
runExecRestyler (ExecRestyler execRestyler) AcceptedJob {..} = do
    now <- liftIO getCurrentTime
    fmap (success now)
        $ withExceptT (failure now)
        $ ExceptT
        $ tryAny
        $ execRestyler ajRepo ajJob
  where
    success now = SucceededExecRestyler . overEntity ajJob . completeJob now
    failure now =
        FailedExecRestyler . overEntity ajJob . completeJobErrored now
