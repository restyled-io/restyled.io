{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Robots
    ( getRobotsR
    ) where

import Restyled.Prelude

import Data.FileEmbed (embedFile)
import Restyled.Foundation
import Restyled.Yesod

getRobotsR :: Handler TypedContent
getRobotsR =
    pure . TypedContent typePlain $ toContent $(embedFile "config/robots.txt")
