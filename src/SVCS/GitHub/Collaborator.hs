{-# LANGUAGE LambdaCase #-}

-- |
--
-- <https://developer.github.com/v3/repos/collaborators/#review-a-users-permission-level>
--
module SVCS.GitHub.Collaborator
    ( githubCollaboratorCanRead
    )
where

import Prelude

import Control.Applicative ((<|>))
import Control.Exception.Safe (tryIO)
import Control.Monad.Except
import Control.Monad.Logger
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString.Lazy as BL
import Data.Functor (($>))
import Data.Semigroup ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding
import GHC.Generics
import GitHub.Data (toPathPart)
import Network.HTTP.Client (responseBody)
import Network.HTTP.Simple
import SVCS.Names

data RepoPermission
    = AdminPermission
    | ReadPermission
    | WritePermission
    | NonePermission

instance FromJSON RepoPermission where
    parseJSON = withText "permission" $ \case
        "admin" -> pure AdminPermission
        "read" -> pure ReadPermission
        "write" -> pure WritePermission
        "none" -> pure NonePermission
        x -> fail $ "Invalid permissions value " <> unpack x

canRead :: RepoPermission -> Bool
canRead AdminPermission = True
canRead ReadPermission = True
canRead WritePermission = True
canRead NonePermission = False

newtype CollaboratorPermissions = CollaboratorPermissions
    { cpPermission :: RepoPermission
    }
    deriving Generic

instance FromJSON CollaboratorPermissions where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ApiErrorDetails = ApiErrorDetails
    { _aedMessage :: Text
    , _aedDocumentationUrl :: Text
    }
    deriving (Show, Generic)

instance FromJSON ApiErrorDetails where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data ApiResponse
    = OK CollaboratorPermissions
    | ErrorDetails ApiErrorDetails

instance FromJSON ApiResponse where
    parseJSON v = (OK <$> parseJSON v) <|> (ErrorDetails <$> parseJSON v)

githubCollaboratorCanRead
    :: (MonadIO m, MonadLogger m)
    => RepoAccessToken
    -> OwnerName
    -> RepoName
    -> GitHubUserName
    -> ExceptT String m Bool
githubCollaboratorCanRead token owner name user = do
    request <-
        githubRequest token
        $ "/repos/"
        <> unpack (toPathPart owner)
        <> "/"
        <> unpack (toPathPart name)
        <> "/collaborators/"
        <> unpack (toPathPart user)
        <> "/permission"

    response <- withExceptT show $ exceptIO $ tryIO $ httpLbs request
    apiResponse <- decodeResponse response

    case apiResponse of
        OK permission -> pure $ canRead $ cpPermission permission
        ErrorDetails details -> warn details
  where
    warn d = logWarnN (pack $ "Collaborators response: " <> show d) $> False

githubRequest
    :: Monad m => RepoAccessToken -> String -> ExceptT String m Request
githubRequest token requestPath = do
    request <-
        withExceptT show
        $ liftEither
        $ parseRequest
        $ "https://api.github.com"
        <> requestPath

    pure $ setRequestHeaders
        [ ("Accept", "application/vnd.github.machine-man-preview+json")
        , ("Authorization", "token " <> encodeUtf8 (unRepoAccessToken token))
        , ("User-Agent", "Restyled.io")
        ]
        request

decodeResponse
    :: (FromJSON a, Monad m) => Response BL.ByteString -> ExceptT String m a
decodeResponse response =
    withExceptT toErrorMessage $ liftEither $ eitherDecode body
  where
    body = responseBody response
    toErrorMessage msg = unlines
        [ "Error decoding JSON response body"
        , "Message: " <> msg
        , "Body: " <> show body
        ]

exceptIO :: MonadIO m => IO (Either e a) -> ExceptT e m a
exceptIO = ExceptT . liftIO
