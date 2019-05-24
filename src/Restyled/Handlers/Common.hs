{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Common
    ( getRevisionR
    , getFaviconR
    , getRobotsR
    )
where

import Restyled.Prelude

import qualified Data.ByteString.Char8 as C8
import Data.FileEmbed (embedFile)
import Development.GitRev (gitCommitDate, gitHash)
import Restyled.Foundation
import Restyled.Settings
import Restyled.Yesod

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
    favicon <- liftIO . C8.readFile =<< getsYesod (appFavicon . view settingsL)
    pure $ TypedContent "image/x-icon" $ toContent favicon

getRobotsR :: Handler TypedContent
getRobotsR =
    pure . TypedContent typePlain $ toContent $(embedFile "config/robots.txt")
