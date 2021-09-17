module Restyled.Backend.Container
    ( StoppedContainer(..)
    , getStoppedContainers
    ) where

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
    containerIds <- getContainerIdsBy
        ["label=restyler", "label=job-id", "status=exited"]

    if null containerIds
        then pure $ Right []
        else inspectContainers containerIds

getContainerIdsBy
    :: ( MonadUnliftIO m
       , MonadReader env m
       , HasLogFunc env
       , HasProcessContext env
       )
    => [String]
    -> m [String]
getContainerIdsBy filters = do
    (ec, out, err) <- proc "docker" args readProcess

    if ec == ExitSuccess
        then pure $ lines $ LBS8.unpack out
        else [] <$ logWarn
            ("getContainerIdsBy: docker-ps exited "
            <> displayShow ec
            <> ", with stderr "
            <> displayShow err
            )
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
inspectContainers containerIds = do
    (ec, out, err) <- proc "docker" ("inspect" : containerIds) readProcess

    if ec == ExitSuccess
        then pure $ eitherDecode out
        else Right [] <$ logWarn
            ("inspectContainers: docker-inspect exited "
            <> displayShow ec
            <> ", with stderr "
            <> displayShow err
            )
