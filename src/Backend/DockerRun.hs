module Backend.DockerRun
    ( dockerRunArgs
    , dockerRunArgsLogged
    )
where

import Backend.Import

dockerRunArgs
    :: AppSettings -> RepoAccessToken -> Repo -> Entity Job -> [String]
dockerRunArgs settings token repo job =
    ["run", "--rm"] <> restylerEnv settings token repo <> restyler settings job

-- | @'dockerRunArgsLogged'@ but without possibly-secret @--env@ arguments
dockerRunArgsLogged :: AppSettings -> Entity Job -> [String]
dockerRunArgsLogged settings job = ["run", "--rm"] <> restyler settings job

restyler :: AppSettings -> Entity Job -> [String]
restyler settings job =
    restylerVolumes <> [restylerImage settings] <> restylerArgs settings job

-- brittany-disable-next-binding

restylerEnv :: AppSettings -> RepoAccessToken -> Repo -> [String]
restylerEnv settings token repo =
    [ "--env", "DEBUG=" <> if repoIsDebug settings repo then "1" else ""
    , "--env", "GITHUB_ACCESS_TOKEN=" <> unpack (unRepoAccessToken token)
    ]

-- brittany-disable-next-binding

restylerVolumes :: [String]
restylerVolumes =
    [ "--volume", "/tmp:/tmp"
    , "--volume", "/var/run/docker.sock:/var/run/docker.sock"
    ]

restylerImage :: AppSettings -> String
restylerImage AppSettings {..} =
    appRestylerImage <> maybe "" (":" <>) appRestylerTag

restylerArgs :: AppSettings -> Entity Job -> [String]
restylerArgs settings job =
    ["--job-url", jobUrl settings job, jobPrSpec $ entityVal job]

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

jobPrSpec :: Job -> String
jobPrSpec Job {..} = unpack $ repoPullPath jobOwner jobRepo jobPullRequest
