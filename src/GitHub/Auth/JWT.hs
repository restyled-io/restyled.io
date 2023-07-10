-- | Extension of @"GitHub.Auth"@ to support JWT-based authorization
module GitHub.Auth.JWT
  ( AuthJWT (..)
  , authJWT
  , authJWTMax
  ) where

import Prelude

import Control.Exception.Safe (throwString)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GHC.Stack
import GitHub.Data
import GitHub.Data.Apps
import Network.HTTP.Simple (addRequestHeader)
import Network.HTTP.Types (hAuthorization)
import qualified Web.JWT as JWT

newtype AuthJWT = AuthJWT ByteString

instance AuthMethod AuthJWT where
  endpoint (AuthJWT _) = Nothing

  setAuthRequest (AuthJWT token) =
    addRequestHeader hAuthorization $ "bearer " <> token

authJWT :: HasCallStack => NominalDiffTime -> Id App -> AppKey -> IO AuthJWT
authJWT expires githubAppId appKey = do
  now <- getCurrentTime
  signer <-
    maybe (throwString "Invalid RSA data") pure
      =<< JWT.rsaKeySecret (unAppKey appKey)

  pure $
    AuthJWT $
      encodeUtf8 $
        JWT.encodeSigned
          signer
          mempty {JWT.alg = Just JWT.RS256}
          mempty
            { JWT.iat = numericDate now
            , JWT.exp = numericDate $ addUTCTime expires now
            , JWT.iss = JWT.stringOrURI $ toPathPart githubAppId
            }

-- | Authorize via JWT, expiring in 9 minutes
--
-- 10 minutes is the documented maximum, but I can reliably trigger a "too far
-- in the future" error, which I assume is due to clocks skew, so we treat 9
-- minutes as the real maximum.
authJWTMax :: HasCallStack => Id App -> AppKey -> IO AuthJWT
authJWTMax = authJWT $ 9 * 60

numericDate :: UTCTime -> Maybe JWT.NumericDate
numericDate = JWT.numericDate . utcTimeToPOSIXSeconds
