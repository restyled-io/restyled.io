module RIO.Process.Follow
    ( followProcess
    , captureFollowedProcess
    , captureFollowedProcessWith
    )
where

import RIO

import RIO.Process
import System.IO (hGetLine)
import System.IO.Error (isEOFError)

-- | Run a process and execute an action on each line of output
followProcess
    :: (HasLogFunc env, HasProcessContext env)
    => FilePath
    -- ^ Command
    -> [String]
    -- ^ Arguments
    -> (String -> RIO env ())
    -- ^ Called with each line of @stdout@
    -> (String -> RIO env ())
    -- ^ Called with each line of @stderr@
    -> RIO env ExitCode
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

-- | Take a @'followProcess'@ and run it capturing its output to a value
captureFollowedProcess
    :: ((String -> RIO env ()) -> (String -> RIO env ()) -> RIO env ExitCode)
    -> RIO env (ExitCode, String, String)
captureFollowedProcess = captureFollowedProcessWith ignore ignore
  where
    ignore :: String -> RIO env ()
    ignore _ = pure ()

-- | Take a @'followProcess'@ and run it capturing its output to a value
--
-- And also executing the given actions.
--
captureFollowedProcessWith
    :: (String -> RIO env ())
    -- ^ Action for @stdout@
    -> (String -> RIO env ())
    -- ^ Action for @stderr@
    -> ( (String -> RIO env ())
       -> (String -> RIO env ())
       -> RIO env ExitCode
       )
    -- ^ @'followProcess'@-like
    -> RIO env (ExitCode, String, String)
captureFollowedProcessWith fOut fErr follow = do
    outRef <- newIORef []
    errRef <- newIORef []

    (,,)
        <$> follow (actAndAppend outRef fOut) (actAndAppend errRef fErr)
        <*> (unlines <$> readIORef outRef)
        <*> (unlines <$> readIORef errRef)

actAndAppend :: MonadIO m => IORef [a] -> (a -> m ()) -> a -> m ()
actAndAppend ref f x = f x <* atomicModifyIORef' ref (\xs -> (xs <> [x], ()))
