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
import qualified Data.Text.IO as T
import qualified Prelude as P

setupGitRepo :: FilePath -> IO ()
setupGitRepo dir = do
    setCurrentDirectory dir
    callProcess "git" ["init", "--quiet"]
    callProcess "git" ["commit", "--quiet", "--allow-empty", "--message", "Test"]

-- | Create a tracked file with the given content
--
-- Assumes you're already in a repository. Commits in branch if given.
--
setupGitTrackedFile :: FilePath -> Text -> Maybe String -> IO ()
setupGitTrackedFile name content mbranch = do
    T.writeFile name content

    for_ mbranch $ \branch ->
        callProcess "git" ["checkout", "--quiet", "-b", branch]

    callProcess "git" ["add", name]
    callProcess "git" ["commit", "--quiet", "--message", "Write code"]

-- | Dedent content
--
-- N.B. assumes an initial "\n" and trailing quote-end, which is what happens
-- when you use the st quasi-quoter in the following style:
--
-- > [st|
-- >   some code
-- >   some code
-- >   |]
--
-- In other words, do not use this on (e.g.) @\"Some text\n\"@
--
dedent :: Text -> Text
dedent t = T.unlines $ map (T.drop $ indent lns) lns
  where
    lns = dropEnd $ T.lines $ T.drop 1 t
    dropEnd = reverse . drop 1 . reverse

    indent [] = 0
    indent ts = P.minimum $ map (T.length . T.takeWhile isSpace) ts

withEmptySystemTempFile :: (FilePath -> IO a) -> IO a
withEmptySystemTempFile = bracket (emptySystemTempFile "") removeFile

hasError :: String -> Either String b -> Bool
hasError msg (Left err) = msg `isInfixOf` err
hasError _ _ = False
