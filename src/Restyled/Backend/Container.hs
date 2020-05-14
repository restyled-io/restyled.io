module Restyled.Backend.Container
    ( StoppedContainer(..)
    , getStoppedContainers
    , RunningContainer(..)
    , getRunningContainer
    , signalContainer
    , signalContainerLogged
    )
where

import Restyled.Prelude

import qualified Data.ByteString.Lazy.Char8 as LBS8
import Restyled.Models

data StoppedContainer = StoppedContainer
    { scJobId :: JobId
    , scContainerId :: String
    , scStartedAt :: UTCTime
    , scFinishedAt :: UTCTime
    , scExitCode :: Int
    }
    deriving (Eq, Show)

instance FromJSON StoppedContainer where
    parseJSON = withObject "Container" $ \o -> do
        state <- o .: "State"
        jobId <-
            either fail pure
            . fromPathPieceEither
            =<< (.: "job-id")
            =<< (.: "Labels")
            =<< (o .: "Config")
        StoppedContainer jobId
            <$> o
            .: "Id"
            <*> state
            .: "StartedAt"
            <*> state
            .: "FinishedAt"
            <*> state
            .: "ExitCode"

data RunningContainer = RunningContainer
    { rcJobId :: JobId
    , rcContainerId :: String
    , rcStartedAt :: UTCTime
    }

instance Display RunningContainer where
    display RunningContainer {..} =
        "RunningContainer "
            <> display (pack rcContainerId)
            <> ", job "
            <> display (toPathPiece rcJobId)
            <> ", started "
            <> displayShow rcStartedAt

instance FromJSON RunningContainer where
    parseJSON = withObject "Container" $ \o -> do
        state <- o .: "State"
        jobId <-
            either fail pure
            . fromPathPieceEither
            =<< (.: "job-id")
            =<< (.: "Labels")
            =<< (o .: "Config")
        RunningContainer jobId <$> o .: "Id" <*> state .: "StartedAt"

fromPathPieceEither :: PathPiece a => Text -> Either String a
fromPathPieceEither x =
    note ("Unable to parse with PathPiece: " <> unpack x) $ fromPathPiece x

getStoppedContainers
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => m (Either String [StoppedContainer])
getStoppedContainers = do
    containerIds <- getContainerIdsBy
        ["label=restyler", "label=job-id", "status=exited"]

    if null containerIds
        then pure $ Right []
        else inspectContainers containerIds

getRunningContainer
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => JobId
    -> m (Either String (Maybe RunningContainer))
getRunningContainer jobId = do
    containerIds <- getContainerIdsBy
        [ "label=restyler"
        , "label=job-id=" <> unpack (toPathPiece jobId)
        , "status=exited"
        ]

    if null containerIds
        then pure $ Right Nothing
        else second listToMaybe <$> inspectContainers containerIds

getContainerIdsBy
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => [String]
    -> m [String]
getContainerIdsBy filters =
    lines . LBS8.unpack <$> proc "docker" args readProcessStdout_
  where
    args = ["ps", "--format", "{{.ID}}"] <> filterArgs
    filterArgs = concatMap (\f -> "--filter" : [f]) filters

inspectContainers
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       , FromJSON a
       )
    => [String]
    -> m (Either String [a])
inspectContainers containerIds =
    eitherDecode <$> proc "docker" ("inspect" : containerIds) readProcessStdout_

signalContainer
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => String
    -> RunningContainer
    -> m ExitCode
signalContainer signal RunningContainer {..} =
    proc "docker" ["kill", "--signal", signal, rcContainerId] runProcess

signalContainerLogged
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => String
    -> RunningContainer
    -> m ()
signalContainerLogged signal container = do
    logDebug $ fromString signal <> " " <> display container
    ec <- signalContainer signal container

    case ec of
        ExitSuccess -> logDebug "Success"
        ExitFailure c -> logWarn $ "Non-zero exit:" <> displayShow c
