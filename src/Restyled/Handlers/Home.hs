{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Home
  ( getHomeR
  ) where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Settings
import Restyled.Widgets.Job (githubActionsWarning)
import Restyled.Yesod

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
  setTitle "Restyled"
  $(widgetFile "homepage")
