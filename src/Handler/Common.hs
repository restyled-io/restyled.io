{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Common
    ( getRevisionR
    , getFaviconR
    , getRobotsR
    )
where

import Import

import qualified Data.ByteString.Char8 as C8
import Data.FileEmbed (embedFile)
import Development.GitRev (gitCommitDate, gitHash)


getRevisionR :: Handler TypedContent
getRevisionR = pure . TypedContent typePlain $ toContent appRevision

-- brittany-disable-next-binding
appRevision :: ByteString
appRevision =
#if DOCKERIZED
    $(embedFile "config/revision")
#else
    $(gitHash) <> " - " <> $(gitCommitDate)
#endif

getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    favicon <- liftIO . C8.readFile =<< getsYesod (appFavicon . appSettings)
    pure $ TypedContent "image/x-icon" $ toContent favicon

getRobotsR :: Handler TypedContent
getRobotsR =
    pure . TypedContent typePlain $ toContent $(embedFile "config/robots.txt")
