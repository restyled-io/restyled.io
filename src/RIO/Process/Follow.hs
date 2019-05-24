module RIO.Process.Follow
    ( followProcess
    , captureFollowedProcess
    , captureFollowedProcessWith

    -- * Minor @"RIO.Process"@ extensions
    , withExtraEnvVars
    )
where

import RIO

import qualified Data.Map as Map
import RIO.Process
import System.IO (hGetLine)
import System.IO.Error (isEOFError)

-- | Run a process and execute an action on each line of output
followProcess
    :: ( HasLogFunc env
       , HasProcessContext env
       , MonadReader env m
       , MonadUnliftIO m
       )
    => FilePath
    -- ^ Command
    -> [String]
    -- ^ Arguments
    -> (String -> m ())
    -- ^ Called with each line of @stdout@
    -> (String -> m ())
    -- ^ Called with each line of @stderr@
    -> m ExitCode
followProcess cmd args fOut fErr =
    proc cmd args $ \pc -> withProcess (setPipes pc) $ \p -> do
        aOut <- async $ followPipe (getStdout p) fOut
        aErr <- async $ followPipe (getStderr p) fErr
        ec <- waitExitCode p
        ec <$ traverse_ wait [aOut, aErr]

setPipes :: ProcessConfig stdin stdout stderr -> ProcessConfig () Handle Handle
setPipes = setStdin closed . setStdout createPipe . setStderr createPipe

followPipe :: MonadUnliftIO m => Handle -> (String -> m ()) -> m ()
followPipe hdl act = loop `catch` handleEOF
  where
    loop = do
        ln <- liftIO $ hGetLine hdl
        act ln
        followPipe hdl act

    handleEOF ex
        | isEOFError ex = pure ()
        | otherwise = throwIO ex

-- | @'captureFollowedProcessWith'@ but not acting on the output at all
--
-- This /is/ @'readProcessWithExitCode'@, but able to re-use something that was
-- defined for @'followProcess'@ because of some other use-case.
--
captureFollowedProcess
    :: MonadIO m
    => ((String -> m ()) -> (String -> m ()) -> m ExitCode)
    -> m (ExitCode, String, String)
captureFollowedProcess = captureFollowedProcessWith ignore ignore

ignore :: Applicative f => a -> f ()
ignore _ = pure ()

-- | Take a @'followProcess'@ and act on and capture its output to a value
--
-- This approximates @'readProcessWithExitCode'@ with an added ability to see
-- the output streams as they happen.
--
captureFollowedProcessWith
    :: MonadIO m
    => (String -> m ())
    -- ^ Action for @stdout@
    -> (String -> m ())
    -- ^ Action for @stderr@
    -> ((String -> m ()) -> (String -> m ()) -> m ExitCode)
    -- ^ @'followProcess'@-like
    -> m (ExitCode, String, String)
captureFollowedProcessWith fOut fErr follow = do
    outRef <- newIORef []
    errRef <- newIORef []

    (,,)
        <$> follow (actAndAppend outRef fOut) (actAndAppend errRef fErr)
        <*> (unlines <$> readIORef outRef)
        <*> (unlines <$> readIORef errRef)

actAndAppend :: MonadIO m => IORef [a] -> (a -> m ()) -> a -> m ()
actAndAppend ref f x = f x <* atomicModifyIORef' ref (\xs -> (xs <> [x], ()))

withExtraEnvVars
    :: (HasProcessContext env, MonadReader env m, MonadIO m)
    => [(Text, Text)]
    -> m a
    -> m a
withExtraEnvVars envs = withModifyEnvVars (<> Map.fromList envs)
