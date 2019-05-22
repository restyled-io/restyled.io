{-# LANGUAGE LambdaCase #-}

module RIO.Logger
    (
    -- * Building @'LogFunc'@
      terminalLogFunc
    , terminalUseColor
    , simpleLogFunc

    -- * Interfacing with @"Control.Monad.Logger"@ types
    , logFuncLog
    )
where

import RIO

import qualified Control.Monad.Logger as Logger
import Data.ByteString.Builder (Builder, byteString, toLazyByteString)
import qualified Data.ByteString.Char8 as BS8
import RIO.Orphans ()
import System.Console.ANSI

-- | @'simpleLogFunc'@ with use-color set automatically
terminalLogFunc :: LogLevel -> IO LogFunc
terminalLogFunc logLevel = simpleLogFunc logLevel <$> terminalUseColor

terminalUseColor :: IO Bool
terminalUseColor = and <$> traverse hSupportsANSI [stdout, stderr]

-- | Construct a simple @'LogFunc'@
--
-- - Output to @'stdout'@
-- - Only show message at or above a given @'LogLevel'@
-- - Use color if told to
--
simpleLogFunc
    :: LogLevel
    -- ^ Minimum @'LogLevel'@
    -> Bool
    -- ^ Use color?
    -> LogFunc
simpleLogFunc logLevel useColor = mkLogFunc $ \_cs _source level msg ->
    when (level >= logLevel)
        $ BS8.putStrLn
        $ toStrictBytes
        $ toLazyByteString
        $ "["
        <> setSGRCodeBuilder [levelStyle level]
        <> levelBuilder level
        <> setSGRCodeBuilder [Reset]
        <> "] "
        <> getUtf8Builder msg
  where
    setSGRCodeBuilder
        | useColor = byteString . BS8.pack . setSGRCode
        | otherwise = const ""

levelBuilder :: LogLevel -> Builder
levelBuilder = byteString . \case
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
