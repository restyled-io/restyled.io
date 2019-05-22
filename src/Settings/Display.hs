{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Settings.Display
    ( displayAppSettings
    )
where

import Restyled.Prelude

import qualified Data.ByteString.Char8 as C8
import Database.Persist.Postgresql (PostgresConf(..))
import Database.Redis (ConnectInfo(..), PortID(..))
import Settings

displayAppSettings :: AppSettings -> String
displayAppSettings AppSettings {..} = concat
    [ "{"
    , " log_level=" <> show appLogLevel
    , " host=" <> show appHost
    , " port=" <> show appPort
    , " root=" <> show appRoot
    , " db=[" <> displayPgConnStr (pgConnStr appDatabaseConf) <> "]"
    , " redis=[" <> displayRedisURL appRedisConf <> "]"
    , " restyler=[" <> displayDockerImage appRestylerImage appRestylerTag <> "]"
    , " }"
    ]

displayPgConnStr :: ByteString -> String
displayPgConnStr bs = "postgres://" <> user <> ":<redacted>" <> rest
  where
    x = drop 2 $ dropWhile (/= '/') $ C8.unpack bs
    user = takeWhile (/= ':') x
    rest = dropWhile (/= '@') x

displayRedisURL :: ConnectInfo -> String
displayRedisURL ConnInfo {..} = "redis://" <> user <> rest
  where
    user = maybe "" (const "<redacted>@") connectAuth
    rest = concat
        [ connectHost
        , ":" <> displayPortID connectPort
        , "/" <> show connectDatabase
        ]

displayPortID :: PortID -> String
displayPortID (Service s) = s
displayPortID (PortNumber p) = show p
displayPortID (UnixSocket s) = s

displayDockerImage :: String -> Maybe String -> String
displayDockerImage image tag = image <> maybe "" (":" <>) tag
