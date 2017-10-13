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
import qualified Network.AWS.Waiter as AWS

data CreateParameters = CreateParameters
    { cpStackName :: Text
    , cpGitHubAppId :: Int
    , cpGitHubAppKey :: FilePath
    , cpDBUsername :: Text
    , cpDBPassword :: Text
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
        (  long "db-username"
        <> help "Master username for RDS instance"
        ))
    <*> (T.pack <$> strOption
        (  long "db-password"
        <> help "Master password for RDS instance"
        ))
    <*> (T.pack <$> strOption
        (  long "redis-url"
        <> metavar "URL"
        <> help "Connection string to Redis instance"
        ))

data StackUpdate
    = TemplateUpdate Text TemplateOptions
    | ParameterUpdate UpdateParameters

data UpdateParameters = UpdateParameters
    { upStackName :: Text
    , upGitHubAppId :: Maybe Int
    , upGitHubAppKey :: Maybe FilePath
    , upDBUsername :: Maybe Text
    , upDBPassword :: Maybe Text
    , upRedisURL :: Maybe Text
    , upImageTag :: Maybe Text
    , upAppServiceCount :: Maybe Int
    , upBackendServiceCount :: Maybe Int
    }

updateTemplateOptions :: Parser StackUpdate
updateTemplateOptions = TemplateUpdate
    <$> (T.pack <$> strOption
        (  long "stack-name"
        <> help "Name of Stack to update"
        ))
    <*> templateOptions

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
        (  long "db-username"
        <> help "Master username for the RDS instance"
        ))
    <*> optional (T.pack <$> strOption
        (  long "db-password"
        <> help "Master password for the RDS instance"
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
                & AWS.pParameterKey ?~ "DBUsername"
                & AWS.pParameterValue ?~ cpDBUsername
            , AWS.parameter
                & AWS.pParameterKey ?~ "DBPassword"
                & AWS.pParameterValue ?~ cpDBPassword
            , AWS.parameter
                & AWS.pParameterKey ?~ "RedisURL"
                & AWS.pParameterValue ?~ cpRedisURL
            ]

    print $ rsp ^. AWS.csrsResponseStatus
    print $ rsp ^. AWS.csrsStackId
    putStrLn "Awaiting..."

    accept <- awaitAWS AWS.stackCreateComplete
        $ AWS.describeStacks
        & AWS.dStackName ?~ cpStackName

    print accept

updateStack :: StackUpdate -> IO ()
updateStack (TemplateUpdate stackName tops) = do
    rsp <- runAWS
        $ AWS.updateStack stackName
        & AWS.usTemplateBody ?~ lbsToText (encodeTemplate $ cfTemplate env)
        & AWS.usParameters .~
            [ toParameter "GitHubAppId" Nothing
            , toParameter "GitHubAppKeyBase64" Nothing
            , toParameter "DBUsername" Nothing
            , toParameter "DBPassword" Nothing
            , toParameter "RedisURL" Nothing
            , toParameter "ImageTag" Nothing
            , toParameter "AppServiceCount" Nothing
            , toParameter "BackendServiceCount" Nothing
            ]

    print =<< awaitUpdate rsp stackName
  where
    env = case tops of
        Staging -> stagingEnv
        Prod -> prodEnv
        Custom e -> e

updateStack (ParameterUpdate UpdateParameters{..}) = do
    key <- mapM readFileBase64 upGitHubAppKey
    rsp <- runAWS
        $ AWS.updateStack upStackName
        & AWS.usUsePreviousTemplate ?~ True
        & AWS.usParameters .~
            [ toParameter "GitHubAppId" $ T.pack . show <$> upGitHubAppId
            , toParameter "GitHubAppKeyBase64" key
            , toParameter "DBUsername" upDBUsername
            , toParameter "DBPassword" upDBPassword
            , toParameter "RedisURL" upRedisURL
            , toParameter "ImageTag" upImageTag
            , toParameter "AppServiceCount" $ T.pack . show <$> upAppServiceCount
            , toParameter "BackendServiceCount" $ T.pack . show <$> upBackendServiceCount
            ]

    print =<< awaitUpdate rsp upStackName

readFileBase64 :: FilePath -> IO Text
readFileBase64 fp = T.filter (not . isSpace) . encodeT <$> T.readFile fp
  where
    encodeT = decodeUtf8 . encode . encodeUtf8

toParameter :: Text -> Maybe Text -> AWS.Parameter
toParameter k Nothing = AWS.parameter
    & AWS.pParameterKey ?~ k
    & AWS.pUsePreviousValue ?~ True
toParameter k (Just v) = AWS.parameter
    & AWS.pParameterKey ?~ k
    & AWS.pParameterValue ?~ v

awaitUpdate :: AWS.UpdateStackResponse -> Text -> IO AWS.Accept
awaitUpdate rsp stackName = do
    print $ rsp ^. AWS.usrsResponseStatus
    print $ rsp ^. AWS.usrsStackId
    putStrLn "Awaiting..."
    awaitAWS AWS.stackUpdateComplete
        $ AWS.describeStacks
        & AWS.dStackName ?~ stackName

lbsToText :: ByteString -> Text
lbsToText = decodeUtf8 . toStrict
