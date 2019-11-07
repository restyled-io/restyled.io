module RIO.Process.Follow
    ( followProcess
    , followProcessStdin
    )
where

import RIO

import RIO.Process
import System.IO (hGetLine)
import System.IO.Error (isEOFError)

-- | Like @'readProcess', but call an action with each line of output
--
-- The actions are run /as output is generated/. This allows (e.g.) capturing
-- process output to persistent storage in a way that could be (mostly)
-- re-constructed in order later.
--
-- @
-- 'proc' "sh" ["-c", "echo hi; sleep 3; echo dee >&2; sleep 2; echo ho"]
--     $ 'followProcess'
--         (\out -> do
--             now <- getCurrentTime
--             insert Log { createdAt = now, stream = "stdout", content = out }
--         )
--         (\err -> do
--             now <- getCurrentTime
--             insert Log { createdAt = now, stream = "stderr", content = out }
--         )
-- @
--
-- After this executes, you can expect @Log@ entries such as
--
-- @
-- | createdAt   | stream | content |
-- |-------------|--------|---------|
-- | {time  0 }  | stdout | hi      |
-- | {time ~3s}  | stderr | dee     |
-- | {time ~5s}  | stdout | ho      |
-- @
--
followProcess
    :: MonadUnliftIO m
    => (String -> m ()) -- ^ Called with each line of @stdout@
    -> (String -> m ()) -- ^ Called with each line of @stderr@
    -> ProcessConfig () stdout stderr
    -> m ExitCode
followProcess = followProcessStdin closed

followProcessStdin
    :: MonadUnliftIO m
    => StreamSpec 'STInput stdin  -- ^ What to do with stdin
    -> (String -> m ()) -- ^ Called with each line of @stdout@
    -> (String -> m ()) -- ^ Called with each line of @stderr@
    -> ProcessConfig stdin stdout stderr
    -> m ExitCode
followProcessStdin stdinSpec fOut fErr pc =
    withProcessWait (setPipes stdinSpec pc) $ \p -> do
        aOut <- async $ followPipe (getStdout p) fOut
        aErr <- async $ followPipe (getStderr p) fErr
        ec <- waitExitCode p
        ec <$ traverse_ wait [aOut, aErr]

setPipes
    :: StreamSpec 'STInput stdin
    -> ProcessConfig stdin stdout stderr
    -> ProcessConfig stdin Handle Handle
setPipes stdinSpec =
    setStdin stdinSpec . setStdout createPipe . setStderr createPipe

followPipe :: MonadUnliftIO m => Handle -> (String -> m ()) -> m ()
followPipe h act = handle handleEOF $ forever $ act =<< liftIO (hGetLine h)
  where
    handleEOF ex
        | isEOFError ex = pure ()
        | otherwise = throwIO ex
