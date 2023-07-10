module Restyled.Handlers.Favicon
  ( getFaviconR
  ) where

import Restyled.Prelude

import qualified Data.ByteString.Char8 as C8
import Restyled.Foundation
import Restyled.Settings
import Restyled.Yesod

getFaviconR :: Handler TypedContent
getFaviconR = do
  cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
  favicon <- liftIO . C8.readFile =<< getsYesod (appFavicon . view settingsL)
  pure $ TypedContent "image/x-icon" $ toContent favicon
