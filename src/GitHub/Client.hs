{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module GitHub.Client
    ( encodeJWT
    , createAccessToken
    , getPullRequest
    , createPullRequest
    , createComment
    ) where

import ClassyPrelude

import Data.Aeson
import Data.Time
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import GitHub.Model
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types
import Safe
import Yesod.Core (PathPiece(..))

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Web.JWT as JWT

data Authorization
    = AuthJWT JWT.JSON
    | AuthToken AccessToken

encodeJWT
    :: GitHubId -- ^ GitHub App ID
    -> Text     -- ^ RSA key content (e.g. read from downloaded @.pem@ file)
    -> IO JWT.JSON
encodeJWT githubAppId pem = do
    now <- getCurrentTime
    key <- fromJustNote "Invalid RSA data" <$> JWT.rsaKeySecret (unpack pem)

    return $ JWT.encodeSigned JWT.RS256 key $ JWT.def
        { JWT.iat = Just $ toNumericDate now
        , JWT.exp = Just $ toNumericDate $ addUTCTime expiration now
        , JWT.iss = Just $ toStringOrURI $ pack $ show $ unGitHubId githubAppId
        }
  where
    expiration = 5 * 60 -- 5 minutes

    toNumericDate t = fromJustNote ("Invalid value for NumericDate: " <> show t)
        $ JWT.numericDate
        $ utcTimeToPOSIXSeconds t

    toStringOrURI x = fromJustNote ("Invalid value for StringOrURI: " <> show x)
        $ JWT.stringOrURI x

createAccessToken :: JWT.JSON -> GitHubId -> IO AccessToken
createAccessToken jwt installationId = do
    let p = "/installations/" <> pack (show $ unGitHubId installationId) <> "/access_tokens"

    -- Type of POST body doesn't matter, just can't be ambigious
    postGitHub (AuthJWT jwt) p (Nothing :: Maybe Value)

getPullRequest :: AccessToken -> RepoFullName -> PRNumber -> IO PullRequest
getPullRequest accessToken repoFullName pullRequestNumber = do
    let p = "/repos/" <> toPathPiece repoFullName
            <> "/pulls/" <> toPathPiece pullRequestNumber

    getGitHub (AuthToken accessToken) p

createPullRequest
    :: AccessToken
    -> RepoFullName
    -> PRTitle
    -> Branch   -- ^ Base branch
    -> Branch   -- ^ Head branch
    -> IO PullRequest
createPullRequest accessToken repoFullName title bBranch hBranch = do
    let p = "/repos/" <> toPathPiece repoFullName <> "/pulls"

    postGitHub (AuthToken accessToken) p $ Just $ object
        [ "title" .= title
        , "base" .= bBranch
        , "head" .= hBranch
        ]

createComment :: AccessToken -> RepoFullName -> PRNumber -> Text -> IO Comment
createComment accessToken repoFullName pullRequestNumber body = do
    let p = "/repos/" <> toPathPiece repoFullName
            <> "/issues/" <> toPathPiece pullRequestNumber
            <> "/comments"

    postGitHub (AuthToken accessToken) p $ Just $ object ["body" .= body]

getGitHub :: FromJSON a => Authorization -> Text -> IO a
getGitHub auth p = do
    request' <- parseRequest $ "https://api.github.com"

    let request = request'
            { path = encodeUtf8 p
            , requestHeaders = authorizationHeader auth : defaultHeaders
            }

    requestJSON request

postGitHub :: (FromJSON a, ToJSON b) => Authorization -> Text -> Maybe b -> IO a
postGitHub auth p mbody = do
    request' <- parseRequest $ "https://api.github.com"

    let request = request'
            { method = "POST"
            , path = encodeUtf8 p
            , requestBody = RequestBodyLBS $ maybe "" encode $ mbody
            , requestHeaders = authorizationHeader auth : defaultHeaders
            }

    requestJSON request

requestJSON :: FromJSON a => Request -> IO a
requestJSON request = do
    mgr <- newManager tlsManagerSettings
    body <- responseBody <$> httpLbs request mgr
    return $ either
        (\e -> error $ unlines
            [ "Error decoding JSON GitHub response"
            , "==================================="
            , "Response body: " ++ L8.unpack body
            , "Error message: " ++ e
            , ""
            ]
        ) id $ eitherDecode $ body

defaultHeaders :: [Header]
defaultHeaders =
    [ (hAccept, "application/vnd.github.machine-man-preview+json")
    , (hUserAgent, "restyled-io")
    ]

authorizationHeader :: Authorization -> Header
authorizationHeader (AuthJWT jwt) = (hAuthorization, "Bearer " <> encodeUtf8 jwt)
authorizationHeader (AuthToken (AccessToken token _)) = (hAuthorization, "token " <> encodeUtf8 token)
