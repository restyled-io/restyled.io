module Restyled.Backend.StoppedContainer
    ( StoppedContainer(..)
    , getStoppedContainerT
    )
where

import Restyled.Prelude

data StoppedContainer = StoppedContainer
    { scStartedAt :: UTCTime
    , scFinishedAt :: UTCTime
    , scExitCode :: Int
    }
    deriving Show

instance FromJSON StoppedContainer where
    parseJSON = withObject "Container" $ \o -> do
        state <- o .: "State"
        running <- state .: "Running"
        when running $ fail "Container not stopped"
        StoppedContainer
            <$> state
            .: "StartedAt"
            <*> state
            .: "FinishedAt"
            <*> state
            .: "ExitCode"

getStoppedContainerT
    :: (HasLogFunc env, HasProcessContext env)
    => String
    -> MaybeT (RIO env) StoppedContainer
getStoppedContainerT containerId = do
    bs <- lift $ proc "docker" ["inspect", containerId] readProcessStdout_

    case eitherDecode bs of
        Left err -> do
            logWarn $ "docker-inspect: " <> fromString err
            hoistMaybe Nothing
        Right xs -> hoistMaybe $ listToMaybe xs
