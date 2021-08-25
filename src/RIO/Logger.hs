module RIO.Logger
    (
    -- * Building @'LogFunc'@
      terminalLogFunc
    , simpleLogFunc

    -- * Interfacing with @"Control.Monad.Logger"@ types
    , logFuncLog

    -- * Low-level helpers
    , logLevelToText
    ) where

import RIO

import qualified Control.Monad.Logger as Logger
import Data.ByteString.Builder (byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import RIO.Orphans ()
import System.Console.ANSI

-- | @'simpleLogFunc'@ with use-color set automatically
terminalLogFunc :: Handle -> LogLevel -> IO LogFunc
terminalLogFunc h logLevel = simpleLogFunc h logLevel <$> hSupportsANSI h

-- | Construct a simple @'LogFunc'@
--
-- - Only show message at or above a given @'LogLevel'@
-- - Use color if told to
--
simpleLogFunc
    :: Handle
    -- ^ Handle for output
    -> LogLevel
    -- ^ Minimum @'LogLevel'@
    -> Bool
    -- ^ Use color?
    -> LogFunc
simpleLogFunc h logLevel useColor = mkLogFunc $ \_cs _source level msg ->
    when (level >= logLevel)
        $ BS8.hPutStrLn h
        $ toStrictBytes
        $ toLazyByteString
        $ setSGRCodeBuilder [levelStyle level]
        <> levelBuilder level
        <> setSGRCodeBuilder [Reset]
        <> " "
        <> getUtf8Builder msg
  where
    setSGRCodeBuilder
        | useColor = byteString . BS8.pack . setSGRCode
        | otherwise = const ""

levelBuilder :: LogLevel -> Builder
levelBuilder = byteString . encodeUtf8 . logLevelToText

levelStyle :: LogLevel -> SGR
levelStyle = \case
    LevelDebug -> SetColor Foreground Dull Magenta
    LevelInfo -> SetColor Foreground Dull Blue
    LevelWarn -> SetColor Foreground Dull Yellow
    LevelError -> SetColor Foreground Dull Red
    LevelOther _ -> Reset

-- | Convert a @'LogFunc'@ to a function of @'MonadLogger'@ arguments
logFuncLog
    :: LogFunc
    -> Logger.Loc
    -> Logger.LogSource
    -> Logger.LogLevel
    -> Logger.LogStr
    -> IO ()
logFuncLog lf loc source level msg =
    runRIO lf $ Logger.monadLoggerLog loc source level msg

logLevelToText :: LogLevel -> Text
logLevelToText = \case
    LevelDebug -> "DEBUG"
    LevelInfo -> "INFO"
    LevelWarn -> "WARN"
    LevelError -> "ERROR"
    LevelOther x -> x
