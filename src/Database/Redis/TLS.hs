module Database.Redis.TLS
  ( ClientParams
  , clientParamsNoVerify
  , parseConnectInfo
  ) where

import Prelude

import Control.Error.Util (note)
import Data.Default.Class (def)
import Data.List (stripPrefix)
import Database.Redis (ConnectInfo (..))
import qualified Database.Redis as Redis
import Network.TLS as TLS
import Network.TLS.Extra.Cipher as TLS

clientParamsNoVerify :: HostName -> ClientParams
clientParamsNoVerify h =
  (TLS.defaultParamsClient h "")
    { TLS.clientSupported =
        def
          { TLS.supportedCiphers = TLS.ciphersuite_default
          }
    , TLS.clientShared = def {TLS.sharedValidationCache = validationCache}
    }
 where
  validationCache =
    TLS.ValidationCache
      (\_ _ _ -> return TLS.ValidationCachePass)
      (\_ _ _ -> return ())

parseConnectInfo
  :: (HostName -> ClientParams) -> String -> Either String ConnectInfo
parseConnectInfo toClientParams url = do
  stripped <- note "Invalid scheme" $ stripPrefix "rediss:" url
  ci <- Redis.parseConnectInfo $ "redis:" <> stripped
  pure $ ci {connectTLSParams = Just $ toClientParams $ connectHost ci}
