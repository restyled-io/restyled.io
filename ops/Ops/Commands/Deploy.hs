{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Ops.Commands.Deploy
    ( DeployOptions(..)
    , deployCommand
    ) where

import Control.Lens
import Control.Monad (void)
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Ops.AWS
import Ops.CloudFormation.Parameters (cfParameters)
import Options.Generic
import Stratosphere (parameterName, unParameters)
import qualified Data.Map as M
import qualified Network.AWS.CloudFormation as AWS

data DeployOptions = DeployOptions
    { doStackName :: Text
    , doImageName :: Maybe Text
    , doImageTag :: Text
    }
    deriving Generic

unPrefixLispCaseModifiers :: String -> Modifiers
unPrefixLispCaseModifiers x = defaultModifiers
    { fieldNameModifier = \y -> fieldNameModifier lispCaseModifiers
        $ fromMaybe y $ stripPrefix x y
    }

instance ParseRecord DeployOptions where
    parseRecord = parseRecordWithModifiers $ unPrefixLispCaseModifiers "do"

deployCommand :: DeployOptions -> IO ()
deployCommand DeployOptions{..} = updateStack doStackName $ withUsePreviousParameters
    [ ("ImageName", doImageName)
    , ("ImageTag", Just doImageTag)
    ]

withUsePreviousParameters :: [(Text, Maybe Text)] -> [(Text, Maybe Text)]
withUsePreviousParameters = M.toList . M.fromList . (knownParameters ++)
  where
    knownParameters = map ((, Nothing) . parameterName) $ unParameters cfParameters

updateStack :: Text -> [(Text, Maybe Text)] -> IO ()
updateStack name params = do
    void $ runAWS
        $ AWS.updateStack name
        & AWS.usUsePreviousTemplate ?~ True
        & AWS.usParameters .~ toParameters params

    putStrLn "Stack updated, awaiting..."
    print =<< awaitAWS AWS.stackCreateComplete
        (AWS.describeStacks & AWS.dStackName ?~ name)
  where
    toParameters = map (uncurry toParameter)
    toParameter k mv = AWS.parameter & AWS.pParameterKey ?~ k & maybe
        (AWS.pUsePreviousValue ?~ True)
        (AWS.pParameterValue ?~) mv
