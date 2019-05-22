module Backend.Job
    ( queueName
    , enqueueJob
    , awaitJob
    , processJob
    )
where

import Backend.Import

import Backend.AcceptedWebhook
import Backend.ExecRestyler
import Backend.Marketplace
import Backend.Webhook

queueName :: ByteString
queueName = "restyled:restyler:jobs"

enqueueJob :: HasRedis env => Entity Job -> RIO env ()
enqueueJob = void . runRedis . lpush queueName . pure . encodeStrict

awaitJob :: HasRedis env => Integer -> RIO env (Maybe (Entity Job))
awaitJob t = do
    eresult <- runRedis $ brpop [queueName] t
    pure $ either (const Nothing) (decodeStrict . snd =<<) eresult

processJob
    :: (HasLogFunc env, HasDB env)
    => ExecRestyler (RIO env)
    -> Entity Job
    -> RIO env ()
processJob execRestyler job = processWebhookFrom execRestyler $ do
    repo <- fetchRepoForAcceptedWebhook $ entityVal job
    allows <- lift $ runDB $ marketplacePlanAllows repo

    pure AcceptedWebhook
        { awRepo = repo
        , awJob = job
        , awMarketplaceAllows = allows
        }

fetchRepoForAcceptedWebhook
    :: HasDB env => Job -> ExceptT IgnoredWebhookReason (RIO env) (Entity Repo)
fetchRepoForAcceptedWebhook job@Job {..} =
    noteT (RepoNotFound jobOwner jobRepo) $ MaybeT $ runDB $ fetchRepoForJob job
