-- | Execution of the Restyler process
module Restyled.Backend.ExecRestyler
    ( ExecRestyler(..)
    , tryExecRestyler
    )
where

import Restyled.Prelude

import Restyled.Backend.AcceptedJob
import Restyled.Models

-- | Execution of the Restyler process
newtype ExecRestyler m = ExecRestyler
    { unExecRestyler
        :: Entity Repo
        -> Entity Job
        -> Maybe (Entity RestyleMachine)
        -> m ExitCode
    }

-- | Run the @'ExecRestyler'@ and capture exceptions to @'ExceptT'@
tryExecRestyler
    :: MonadUnliftIO m
    => ExecRestyler m
    -> AcceptedJob
    -> Maybe (Entity RestyleMachine)
    -> ExceptT SomeException m ExitCode
tryExecRestyler (ExecRestyler execRestyler) AcceptedJob {..} =
    ExceptT . tryAny . execRestyler ajRepo ajJob
