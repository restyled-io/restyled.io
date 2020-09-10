module Restyled.Backend.StoppedContainer
    ( StoppedContainer(..)
    , getStoppedContainers
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
    deriving stock (Eq, Show)

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
    containerIds <- getExitedContainerIds

    if null containerIds
        then pure $ Right []
        else do
            bs <- proc "docker" ("inspect" : containerIds) readProcessStdout_
            pure $ eitherDecode bs

-- brittany-disable-next-binding

getExitedContainerIds
    :: (MonadUnliftIO m, MonadReader env m, HasLogFunc env, HasProcessContext env)
    => m [String]
getExitedContainerIds = lines . LBS8.unpack <$> proc "docker"
    [ "ps"
    , "--filter", "label=restyler"
    , "--filter", "label=job-id"
    , "--filter", "status=exited"
    , "--format", "{{.ID}}"
    ]
    readProcessStdout_
