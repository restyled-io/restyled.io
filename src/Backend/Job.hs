module Backend.Job
    ( queueName
    , enqueueJob
    , awaitJob
    , processJob
    )
where

import Backend.Import

import Backend.AcceptedJob
import Backend.ExecRestyler

queueName :: ByteString
queueName = "restyled:restyler:jobs"

enqueueJob :: HasRedis env => Entity Job -> RIO env ()
enqueueJob = void . runRedis . lpush queueName . pure . encodeStrict

awaitJob :: HasRedis env => Integer -> RIO env (Maybe (Entity Job))
awaitJob t = do
    eresult <- runRedis $ brpop [queueName] t
    pure $ either (const Nothing) (decodeStrict . snd =<<) eresult

processJob :: HasDB env => ExecRestyler (RIO env) -> Entity Job -> RIO env ()
processJob execRestyler jobE@(Entity _ job) = do
    result <- runExceptT $ do
        repo <- noteT "Repo not found" $ MaybeT $ runDB $ fetchRepoForJob job
        withExceptT show $ tryExecRestyler execRestyler $ AcceptedJob
            { ajRepo = repo
            , ajJob = jobE
            }

    now <- liftIO getCurrentTime

    let failure :: String -> Entity Job -> Entity Job
        failure = flip overEntity . completeJobErrored now

        success :: ExitCode -> Entity Job -> Entity Job
        success = flip overEntity . completeJob now

    runDB $ replaceEntity $ either failure success result jobE
