{-# LANGUAGE NoImplicitPrelude #-}
module SpecHelper
    ( module SpecHelper
    , module X
    )
    where

import ClassyPrelude as X

import Data.Char (isSpace)
import System.Directory (removeFile, setCurrentDirectory)
import System.IO.Temp as X (emptySystemTempFile, withSystemTempDirectory)
import System.Process as X (callProcess, readProcess)
import Test.Hspec as X
import Text.Shakespeare.Text as X (st)
import qualified Data.Text as T
import qualified Prelude as P

setupGitRepo :: FilePath -> IO ()
setupGitRepo dir = do
    setCurrentDirectory dir
    callProcess "git" ["init", "--quiet"]
    callProcess "git" ["commit", "--quiet", "--allow-empty", "--message", "Test"]

dedent :: Text -> Text
dedent t = T.unlines $ map (T.drop $ indent lns) lns
  where
    lns = T.lines $ T.drop 1 t -- assumes an initial "\n"

    indent [] = 0
    indent ts = P.minimum $ map (T.length . T.takeWhile isSpace) ts

withEmptySystemTempFile :: (FilePath -> IO a) -> IO a
withEmptySystemTempFile = bracket (emptySystemTempFile "") removeFile

hasError :: String -> Either String b -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False
