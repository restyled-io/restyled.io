{-# LANGUAGE LambdaCase #-}

module RIO.Logger
    (
    -- * Building @'LogFunc'@
      terminalLogFunc
    , simpleLogFunc

    -- * Interfacing with @"Control.Monad.Logger"@ types
    , loggerLogLevel
    , logFuncLog
    )
where

import RIO

import qualified Control.Monad.Logger as Logger
import Data.ByteString (ByteString)
import Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import RIO.Orphans ()
import System.Console.ANSI

-- | @'simpleLogFunc'@ with use-color set based on if we're a terminal
terminalLogFunc :: LogLevel -> IO LogFunc
terminalLogFunc logLevel = do
    useColor <- and <$> traverse hSupportsANSI [stdout, stderr]
    pure $ simpleLogFunc logLevel useColor

-- | Construct a simple @'LogFunc'@
simpleLogFunc
    :: LogLevel -- ^ Minimum @'LogLevel'@
    -> Bool -- ^ Use color?
    -> LogFunc
simpleLogFunc logLevel useColor = mkLogFunc $ \_cs _source level msg ->
    when (level >= logLevel) $ do
        BS8.putStr "["
        when useColor $ setSGR [levelStyle level]
        BS8.putStr $ levelStr level
        when useColor $ setSGR [Reset]
        BS8.putStr "] "
        BS8.putStrLn $ toStrictBytes $ toLazyByteString $ getUtf8Builder msg

levelStr :: LogLevel -> ByteString
levelStr = \case
    LevelDebug -> "Debug"
    LevelInfo -> "Info"
    LevelWarn -> "Warn"
    LevelError -> "Error"
    LevelOther x -> encodeUtf8 x

levelStyle :: LogLevel -> SGR
levelStyle = \case
    LevelDebug -> SetColor Foreground Dull Magenta
    LevelInfo -> SetColor Foreground Dull Blue
    LevelWarn -> SetColor Foreground Dull Yellow
    LevelError -> SetColor Foreground Dull Red
    LevelOther _ -> Reset

logFuncLog
    :: LogFunc
    -> Logger.Loc
    -> Logger.LogSource
    -> Logger.LogLevel
    -> Logger.LogStr
    -> IO ()
logFuncLog lf loc source level msg =
    runRIO lf $ Logger.monadLoggerLog loc source level msg

loggerLogLevel :: Logger.LogLevel -> LogLevel
loggerLogLevel = \case
    Logger.LevelDebug -> LevelDebug
    Logger.LevelInfo -> LevelInfo
    Logger.LevelWarn -> LevelWarn
    Logger.LevelError -> LevelError
    Logger.LevelOther x -> LevelOther x
