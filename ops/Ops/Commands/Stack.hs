{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Ops.Commands.Stack
    ( CreateParameters
    , createParameterOptions
    , createStack
    , StackUpdate
    , updateTemplateOptions
    , updateParameterOptions
    , updateStack
    ) where

import Control.Lens
import Data.ByteString.Base64 (encode)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Char (isSpace)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Ops.AWS
import Ops.CloudFormation.Environment
import Ops.CloudFormation.Template
import Ops.Commands.Template
import Options.Applicative
import Stratosphere
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.AWS.CloudFormation as AWS

data CreateParameters = CreateParameters
    { cpStackName :: Text
    , cpGitHubAppId :: Int
    , cpGitHubAppKey :: FilePath
    , cpDatabaseURL :: Text
    , cpRedisURL :: Text
    }

createParameterOptions :: Parser CreateParameters
createParameterOptions = CreateParameters
    <$> (T.pack <$> strOption
        (  long "stack-name"
        <> help "Name of created Stack"
        ))
    <*> option auto
        (  long "github-app-id"
        <> help "GitHub App Id"
        )
    <*> strOption
        (  long "github-app-key"
        <> metavar "PATH"
        <> help "Path to PEM key file for GitHub App"
        )
    <*> (T.pack <$> strOption
        (  long "database-url"
        <> metavar "URL"
        <> help "Connection string to PostgreSQL instance"
        ))
    <*> (T.pack <$> strOption
        (  long "redis-url"
        <> metavar "URL"
        <> help "Connection string to Redis instance"
        ))

data StackUpdate
    = TemplateUpdate TemplateOptions
    | ParameterUpdate UpdateParameters

data UpdateParameters = UpdateParameters
    { upStackName :: Text
    , upGitHubAppId :: Maybe Int
    , upGitHubAppKey :: Maybe FilePath
    , upDatabaseURL :: Maybe Text
    , upRedisURL :: Maybe Text
    , upImageTag :: Maybe Text
    , upAppServiceCount :: Maybe Int
    , upBackendServiceCount :: Maybe Int
    }

updateTemplateOptions :: Parser StackUpdate
updateTemplateOptions = TemplateUpdate <$> templateOptions

updateParameterOptions :: Parser StackUpdate
updateParameterOptions = ParameterUpdate <$>
    ( UpdateParameters
    <$> (T.pack <$> strOption
        (  long "stack-name"
        <> help "Name of Stack to update"
        ))
    <*> optional (option auto
        (  long "github-app-id"
        <> help "GitHub App Id"
        ))
    <*> optional (strOption
        (  long "github-app-key"
        <> metavar "PATH"
        <> help "Path to PEM key file for GitHub App"
        ))
    <*> optional (T.pack <$> strOption
        (  long "database-url"
        <> metavar "URL"
        <> help "Connection string to PostgreSQL instance"
        ))
    <*> optional (T.pack <$> strOption
        (  long "redis-url"
        <> metavar "URL"
        <> help "Connection string to Redis instance"
        ))
    <*> optional (T.pack <$> strOption
        (  long "image-tag"
        <> help "Image tag for deployed Apps services"
        ))
    <*> optional (option auto
        (  long "app-count"
        <> help "Number of App services to run"
        ))
    <*> optional (option auto
        (  long "backend-count"
        <> help "Number of Backend services to run"
        ))
    )

createStack :: TemplateOptions -> CreateParameters -> IO ()
createStack Staging = createStack' stagingEnv
createStack Prod = createStack' prodEnv
createStack (Custom env) = createStack' env

createStack' :: Environment -> CreateParameters -> IO ()
createStack' env CreateParameters{..} = do
    key <- readFileBase64 cpGitHubAppKey
    rsp <- runAWS
        $ AWS.createStack cpStackName
        & AWS.csTemplateBody ?~ lbsToText (encodeTemplate $ cfTemplate env)
        & AWS.csParameters .~
            [ AWS.parameter
                & AWS.pParameterKey ?~ "GitHubAppId"
                & AWS.pParameterValue ?~ T.pack (show cpGitHubAppId)
            , AWS.parameter
                & AWS.pParameterKey ?~ "GitHubAppKeyBase64"
                & AWS.pParameterValue ?~ key
            , AWS.parameter
                & AWS.pParameterKey ?~ "DatabaseURL"
                & AWS.pParameterValue ?~ cpDatabaseURL
            , AWS.parameter
                & AWS.pParameterKey ?~ "RedisURL"
                & AWS.pParameterValue ?~ cpRedisURL
            ]

    putStrLn "Stack create complete"
    putStrLn $ "Response Status: " ++ show (rsp ^. AWS.csrsResponseStatus)
    putStrLn $ "       Stack Id: " ++ show (rsp ^. AWS.csrsStackId)

updateStack :: StackUpdate -> IO ()
updateStack (TemplateUpdate _) = undefined
updateStack (ParameterUpdate UpdateParameters{..}) = do
    key <- mapM readFileBase64 upGitHubAppKey
    rsp <- runAWS
        $ AWS.updateStack upStackName
        & AWS.usUsePreviousTemplate ?~ True
        & AWS.usParameters .~
            [ toParameter "GitHubAppId" $ T.pack . show <$> upGitHubAppId
            , toParameter "GitHubAppKeyBase64" key
            , toParameter "DatabaseURL" upDatabaseURL
            , toParameter "RedisURL" upRedisURL
            , toParameter "ImageTag" upImageTag
            , toParameter "AppServiceCount" $ T.pack . show <$> upAppServiceCount
            , toParameter "BackendServiceCount" $ T.pack . show <$> upBackendServiceCount
            ]

    putStrLn "Stack update complete"
    putStrLn $ "Response Status: " ++ show (rsp ^. AWS.usrsResponseStatus)
    putStrLn $ "       Stack Id: " ++ show (rsp ^. AWS.usrsStackId)
  where
    toParameter :: Text -> Maybe Text -> AWS.Parameter
    toParameter k Nothing = AWS.parameter
        & AWS.pParameterKey ?~ k
        & AWS.pUsePreviousValue ?~ True
    toParameter k (Just v) = AWS.parameter
        & AWS.pParameterKey ?~ k
        & AWS.pParameterValue ?~ v

readFileBase64 :: FilePath -> IO Text
readFileBase64 fp = T.filter isSpace . encodeT <$> T.readFile fp
  where
    encodeT = decodeUtf8 . encode . encodeUtf8

lbsToText :: ByteString -> Text
lbsToText = decodeUtf8 . toStrict
