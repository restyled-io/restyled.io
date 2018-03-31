{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
-- | Common handler functions.
module Handler.Common where

import Import

import Data.FileEmbed (embedFile)
import Development.GitRev (gitCommitDate, gitHash)

getRevisionR :: Handler TypedContent
getRevisionR = TypedContent typePlain . toContent <$> readRevision
  where
    -- We add a static config/revision in docker builds, but want to fall back
    -- to dynamic git operation sin develompent
    readRevision = either
        (\_ -> $(gitHash) <> " - " <> $(gitCommitDate))
        decodeUtf8 <$> tryIO (readFile "config/revision")

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
