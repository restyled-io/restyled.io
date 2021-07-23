module Restyled.Backend.DockerRunArgs
    ( dockerRunArgs
    ) where

import Restyled.Prelude

import Restyled.Models
import Restyled.RestylerImage
import Restyled.Settings

-- brittany-disable-next-binding

dockerRunArgs :: AppSettings -> AccessToken -> Repo -> Entity Job -> [String]
dockerRunArgs settings token repo job =
    [ "--net", "user-bridge"
    , "--log-driver=awslogs"
    , "--log-opt awslogs-region=us-east-1"
    , "--log-opt awslogs-group=" <> unpack (appRestylerLogGroup settings)
    , "--log-opt awslogs-stream="
        <> unpack (appRestylerLogStreamPrefix settings)
        <> unpack (toPathPiece $ entityKey job)
    , "--label", "restyler"
    , "--label", "job-id=" <> unpack (toPathPiece $ entityKey job)
    , "--env", "DEBUG=" <> if repoIsDebug settings repo then "1" else ""
    , "--env", "GITHUB_ACCESS_TOKEN=" <> unpack (atToken token)
    , "--env", "STATSD_HOST=dd-agent"
    , "--volume", "/tmp:/tmp"
    , "--volume", "/var/run/docker.sock:/var/run/docker.sock"
    , image
    , "--job-url", jobUrl settings job, jobPrSpec job
    ]
 where
    image = unpack
        $ unRestylerImage
        $ fromMaybe (appRestylerImage settings)
        $ repoRestylerImage repo

jobUrl :: AppSettings -> Entity Job -> String
jobUrl AppSettings {..} (Entity jobId Job {..}) =
    unpack
        $ appRoot
        <> "/gh/"
        <> toPathPiece jobOwner
        <> "/repos/"
        <> toPathPiece jobRepo
        <> "/jobs/"
        <> toPathPiece jobId

jobPrSpec :: Entity Job -> String
jobPrSpec (Entity _ Job {..}) =
    unpack $ repoPullPath jobOwner jobRepo jobPullRequest
