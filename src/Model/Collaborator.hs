{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model.Collaborator
    ( collaboratorCanRead
    ) where

import Import.NoFoundation hiding (httpLbs)

import Control.Error.Util (note)
import Control.Monad.Except
import Data.Aeson
import Data.Aeson.Casing
import qualified Data.ByteString.Lazy as BL
import GitHub.Data (Id, toPathPart)
import GitHub.Endpoints.Installations (AccessToken(..), App, createAccessToken)
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
    :: (MonadIO m, MonadLogger m) => Id App -> Text -> Repo -> User -> m Bool
collaboratorCanRead appId pem Repo {..} User {..} = do
    result <- runExceptT $ do
        username <- liftEither $ note "No GitHub username" userGithubUsername
        token <- exceptIO $ createAccessToken appId pem repoInstallationId

        request <-
            githubRequest token
            $ "/repos/"
            <> unpack (toPathPart repoOwner)
            <> "/"
            <> unpack (toPathPart repoName)
            <> "/collaborators/"
            <> unpack (toPathPart username)
            <> "/permission"

        response <- withExceptT show $ exceptIO $ tryIO $ httpLbs request
        apiResponse <- decodeResponse response

        case apiResponse of
            OK permission -> pure $ canRead $ cpPermission permission
            ErrorDetails details -> errDetails details

    either err pure result
  where
    err e = logWarnN ("Error authorizing repository:\n" <> pack e) $> False
    errDetails d = logDebugN ("Collaborators response: " <> tshow d) $> False

githubRequest :: Monad m => AccessToken -> String -> ExceptT String m Request
githubRequest token requestPath = do
    request <-
        withExceptT show
        $ liftEither
        $ parseRequest
        $ "http://api.github.com"
        <> requestPath

    pure $ setRequestHeaders
        [ ("Accept", "application/vnd.github.hellcat-preview+json")
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
