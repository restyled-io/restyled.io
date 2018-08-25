{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
--
-- <https://developer.github.com/v3/repos/collaborators/#review-a-users-permission-level>
--
module Model.Collaborator
    ( collaboratorCanRead
    ) where

import Import.NoFoundation hiding (httpLbs)

import Control.Error.Util (note)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString.Lazy as BL
import GitHub.Data.AccessTokens
import GitHub.Endpoints.Installations
import Network.HTTP.Simple

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

collaboratorCanRead
    :: (MonadIO m, MonadLogger m) => AppSettings -> Repo -> User -> m Bool
collaboratorCanRead settings Repo {..} User {..} = do
    result <- runExceptT $ do
        username <- liftEither $ note "No GitHub username" userGithubUsername
        token <- exceptIO $ createAccessToken
            (appGitHubAppId settings)
            (appGitHubAppKey settings)
            repoInstallationId

        request <-
            githubRequest token
            $ "/repos/"
            <> unpack (toPathPiece repoOwner)
            <> "/"
            <> unpack (toPathPiece repoName)
            <> "/collaborators/"
            <> unpack (toPathPiece username)
            <> "/permission"

        response <- withExceptT show $ exceptIO $ tryIO $ httpLbs request
        apiResponse <- decodeResponse response

        case apiResponse of
            OK permission -> pure $ canRead $ cpPermission permission
            ErrorDetails details -> warn details

    either err pure result
  where
    err e = logErrorN ("Error authorizing repository:\n" <> pack e) $> False
    warn d = logWarnN ("Collaborators response: " <> tshow d) $> False

githubRequest :: Monad m => AccessToken -> String -> ExceptT String m Request
githubRequest token requestPath = do
    request <-
        withExceptT show
        $ liftEither
        $ parseRequest
        $ "https://api.github.com"
        <> requestPath

    pure $ setRequestHeaders
        [ ("Accept", "application/vnd.github.machine-man-preview+json")
        , ("Authorization", "token " <> encodeUtf8 (atToken token))
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
