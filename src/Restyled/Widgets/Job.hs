{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Restyled.Widgets.Job
  ( githubActionsWarning
  , jobCard
  , jobListItem
  , jobOutput
  , jobLogLine
  ) where

import Restyled.Prelude hiding (Key)

import Control.Monad.Logger.Aeson (LoggedMessage (..))
import Data.Aeson.Key (Key)
import qualified Data.Aeson.Key as Key
import Data.Aeson.KeyMap (KeyMap)
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Text as T
import qualified Data.Vector as V
import Formatting (format)
import Formatting.Time (diff)
import Restyled.Foundation
import Restyled.Job.Completion
import Restyled.Models hiding (jobLogLine)
import Restyled.Routes
import Restyled.Settings
import Restyled.Widgets.ContainsURLs
import Restyled.Yesod
import Text.Julius (RawJS (..))

githubActionsWarning :: Widget
githubActionsWarning = $(widgetFile "widgets/github-actions-warning")

jobCard :: Entity Job -> Widget
jobCard jobE@(Entity jobId job) = do
  now <- liftIO getCurrentTime
  $(widgetFile "widgets/job-card")

jobListItem :: Entity Job -> Widget
jobListItem (Entity jobId job) = do
  now <- liftIO getCurrentTime
  $(widgetFile "widgets/job-list-item")

jobOutput :: Entity Job -> Widget
jobOutput (Entity jobId job) = $(widgetFile "widgets/job-output")
 where
  streamElementId = "logs-job-id-" <> toPathPiece jobId

jobLogLine :: JobLogLine -> Widget
jobLogLine ln = $(widgetFile "widgets/log-line")
 where
  lm = jobLogLineContentJSON ln
  level = case loggedMessageLevel lm of
    LevelDebug -> "debug"
    LevelInfo -> "info"
    LevelWarn -> "warn"
    LevelError -> "error"
    LevelOther x -> x

padTo :: Int -> Text -> Text
padTo n t = t <> T.replicate pad " " where pad = max 0 $ n - T.length t

renderKeyMap :: KeyMap Value -> Widget
renderKeyMap = mconcat . map (uncurry pair) . KeyMap.toList
 where
  pair :: Key -> Value -> Widget
  pair k v =
    [whamlet|$newline never
        \ #
        <span .key-map-key>#{Key.toText k}
        =
        <span .key-map-value>#{plainValue v}
    |]

  plainValue :: Value -> Text
  plainValue = \case
    Object km ->
      mconcat
        [ "{"
        , T.intercalate ","
            $ map (\(k, v) -> Key.toText k <> ":" <> plainValue v)
            $ KeyMap.toList km
        , "}"
        ]
    Array vs ->
      mconcat
        ["[", T.intercalate "," $ map plainValue $ V.toList vs, "]"]
    String t -> pack $ show t -- encode escapes and quote
    Bool b -> if b then "true" else "false"
    Number n -> dropSuffix ".0" $ show n
    Null -> "null"

dropSuffix :: Text -> Text -> Text
dropSuffix suffix x = fromMaybe x $ T.stripSuffix suffix x
