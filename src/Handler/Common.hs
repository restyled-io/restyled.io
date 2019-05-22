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

getRevisionR :: Handler TypedContent
getRevisionR = pure . TypedContent typePlain $ toContent appRevision

getFaviconR :: Handler TypedContent
getFaviconR = do
    cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
    favicon <- liftIO . C8.readFile =<< getsYesod (appFavicon . appSettings)
    pure $ TypedContent "image/x-icon" $ toContent favicon

getRobotsR :: Handler TypedContent
getRobotsR =
    pure . TypedContent typePlain $ toContent $(embedFile "config/robots.txt")
