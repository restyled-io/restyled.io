module Import
    ( module Import
    )
where

import Foundation as Import
import Import.NoFoundation as Import
import Routes as Import

import Backend.Foundation

-- | Run a backend action from a Handler (e.g. enqueuing a job)
--
-- Uses the @'App'@'s settings, connections, and logger
--
runBackendHandler :: ReaderT Backend (LoggingT Handler) a -> Handler a
runBackendHandler f = do
    app <- getYesod
    runBackendApp app f

-- | Extracted so @'runBackendTest'@ can use it in tests
runBackendApp :: App -> ReaderT Backend (LoggingT m) a -> m a
runBackendApp app@App {..} f = runLoggingT
    (runReaderT
        f
        Backend
            { backendSettings = appSettings
            , backendConnPool = appConnPool
            , backendRedisConn = appRedisConn
            }
    )
    (messageLoggerSource app appLogger)
