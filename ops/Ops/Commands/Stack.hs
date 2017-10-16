{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Ops.Commands.Stack
    ( StackOptions(..)
    , stackOptions
    , createStack
    , updateStack
    ) where

import Control.Lens hiding (argument)
import Control.Monad (forM, void)
import Data.Bifunctor (second)
import Data.ByteString.Base64 (encode)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Char (isSpace)
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Ops.AWS
import Ops.CloudFormation.Parameters
import Ops.CloudFormation.Template
import Options.Applicative
import Stratosphere hiding (argument)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.AWS.CloudFormation as AWS

data ParameterValue
    = ParameterValue Text
    | UsePreviousValue

data StackOptions = StackOptions
    { soStackName :: Text
    , soParameters :: M.Map Text ParameterValue
    }

stackOptions :: Parser StackOptions
stackOptions = StackOptions
    <$> (T.pack <$> strOption
        (long "stack-name" <> metavar "NAME"))
    <*> (M.fromList <$> many (argument (eitherReader readKeyValue)
        (metavar "KEY=VALUE" <> help parameterNamesList)))
  where
    parameterNamesList = unlines
        $ replace "GitHubAppKeyBase64" "GitHubAppKey"
        $ map toExample $ unParameters cfParameters

    replace _ _ [] = []
    replace a b (x:xs)
        | a == x = b : replace a b xs
        | otherwise = x : replace a b xs

    toExample p = T.unpack (parameterName p) ++
        maybe "" (("=" ++) . T.unpack . toText) (parameterDefault' p)

-- | Read @KEY=VALUE@ to @(Key, 'ParameterValue' VALUE)@
readKeyValue :: String -> Either String (Text, ParameterValue)
readKeyValue arg
    | null arg = Left "argument cannot be empty"
    | otherwise = Right (key, ParameterValue val)
  where
    (key, val) = second (T.drop 1) $ T.breakOn "=" $ T.pack arg

createStack :: StackOptions -> IO ()
createStack StackOptions{..} = do
    prs <- replaceGitHubAppKey soParameters

    void $ runAWS
        $ AWS.createStack soStackName
        & AWS.csTemplateBody ?~ lbsToText (encodeTemplate cfTemplate)
        & AWS.csParameters .~ toAWSParameters prs

    putStrLn "Stack created, awaiting..."
    print =<< awaitAWS AWS.stackCreateComplete
        (AWS.describeStacks & AWS.dStackName ?~ soStackName)

updateStack :: StackOptions -> IO ()
updateStack StackOptions{..} = do
    prs <- replaceGitHubAppKey soParameters

    void $ runAWS
        $ AWS.updateStack soStackName
        & AWS.usTemplateBody ?~ lbsToText (encodeTemplate cfTemplate)
        & AWS.usParameters .~ toAWSParameters
            (prs `M.union` usePreviousParameters)

    putStrLn "Stack updated, awaiting..."
    print =<< awaitAWS AWS.stackCreateComplete
        (AWS.describeStacks & AWS.dStackName ?~ soStackName)
  where
    -- | A map of all known parameters with @'UsePreviousValue'@ as the value
    --
    -- The map of passed parameters is unioned over these so that any omitted
    -- parameters will naturally retain their previous values.
    --
    usePreviousParameters :: M.Map Text ParameterValue
    usePreviousParameters = M.fromList
        $ map ((, UsePreviousValue) . parameterName)
        $ unParameters cfParameters

replaceGitHubAppKey :: M.Map Text ParameterValue -> IO (M.Map Text ParameterValue)
replaceGitHubAppKey m = do
    mcontents <- forM (M.lookup "GitHubAppKey" m) $ \case
        ParameterValue fp -> ParameterValue <$> readFileBase64 (T.unpack fp)
        UsePreviousValue -> return UsePreviousValue

    return
        $ maybe id (M.insert "GitHubAppKeyBase64") mcontents
        $ M.delete "GitHubAppKey" m

readFileBase64 :: FilePath -> IO Text
readFileBase64 = (T.filter (not . isSpace) . liftT encode <$>) . T.readFile
  where
    liftT f = decodeUtf8 . f . encodeUtf8

toAWSParameters :: M.Map Text ParameterValue -> [AWS.Parameter]
toAWSParameters = map (uncurry toParameter) . M.toList
  where
    toParameter k (ParameterValue v) = AWS.parameter
        & AWS.pParameterKey ?~ k
        & AWS.pParameterValue ?~ v
    toParameter k UsePreviousValue = AWS.parameter
        & AWS.pParameterKey ?~ k
        & AWS.pUsePreviousValue ?~ True

lbsToText :: ByteString -> Text
lbsToText = decodeUtf8 . toStrict
