module Restyled.Backend.DockerRunArgs
    ( dockerRunArgs
    )
where

import Restyled.Prelude

import Restyled.Models
import Restyled.Settings

-- brittany-disable-next-binding

dockerRunArgs :: AppSettings -> AccessToken -> Repo -> Entity Job -> [String]
dockerRunArgs settings token repo job =
    [ "--label", "restyler"
    , "--env", "DEBUG=" <> if repoIsDebug settings repo then "1" else ""
    , "--env", "GITHUB_ACCESS_TOKEN=" <> unpack (atToken token)
    , "--volume", "/tmp:/tmp"
    , "--volume", "/var/run/docker.sock:/var/run/docker.sock"
    , appRestylerImage settings <> maybe "" (":" <>) (appRestylerTag settings)
    , "--job-url", jobUrl settings job, jobPrSpec job
    ]

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
