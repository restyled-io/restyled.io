module Restyled.Backend.MissingDaemonError
    ( MissingDaemonError(..)
    , toMissingDaemonError
    , onMissingDaemonError
    ) where

import Restyled.Prelude

import Restyled.Backend.RestyleMachine

newtype MissingDaemonError = MissingDaemonError
    { unMissingDaemonError :: ExitCodeException
    }
    deriving stock Show
    deriving newtype Exception

instance Display MissingDaemonError where
    display = fromString . displayException

toMissingDaemonError :: ExitCodeException -> Maybe MissingDaemonError
toMissingDaemonError ex = do
    guard $ eceExitCode ex == ExitFailure 125
    pure $ MissingDaemonError ex

onMissingDaemonError
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , HasSqlPool env
       )
    => MissingDaemonError
    -> m ExitCode
onMissingDaemonError ex = runDB $ do
    lift $ logDebug "MissingDaemonError"
    mRestyleMachineId <- entityKey <$$> getCurrentRestyleMachine

    lift
        $ logDebug
        $ "Current Restyle Machine: "
        <> maybe "<unknown>" (display . toPathPiece) mRestyleMachineId

    for_ mRestyleMachineId $ \machineId -> do
        wasDisabled <- disableRestyleMachine machineId

        lift $ logDebug $ "Disabled: " <> displayShow wasDisabled
        when wasDisabled
            $ lift
            $ logWarn
            $ "Disabled current Restyle Machine ("
            <> display ex
            <> ")"

    pure $ ExitFailure 99
