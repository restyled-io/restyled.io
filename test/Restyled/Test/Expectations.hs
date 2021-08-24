module Restyled.Test.Expectations
    ( shouldMatchJson
    , shouldRedirectTo
    , expectJust
    , expectRight
    , expectationFailure
    , module X
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import Restyled.Foundation
import qualified Test.Hspec.Expectations.Json as HSpec
import Test.Hspec.Lifted as X hiding (expectationFailure)
import qualified Test.Hspec.Lifted as HSpec
import Yesod.Test

shouldMatchJson :: (HasCallStack, MonadIO m) => Value -> Value -> m ()
shouldMatchJson a b = liftIO $ HSpec.shouldMatchJson a b

shouldRedirectTo
    :: YesodExample App a -- ^ Perform the request
    -> Text -- ^ Path to expect a redirect to, e.g. @/auth/login@
    -> YesodExample App ()
shouldRedirectTo doRequest path = do
    void doRequest

    -- This would be more explicit and ought to work, but is always a parse
    -- failure. Sigh.
    -- statusIs 303
    -- location <- getLocation
    -- location `shouldBe` Right route

    result <- followRedirect
    result `shouldSatisfy` predicate
  where
    predicate = \case
        Left _err -> False
        Right url -> path `T.isSuffixOf` url

expectJust :: (HasCallStack, MonadIO m) => String -> Maybe a -> m a
expectJust item = \case
    Nothing -> expectationFailure $ "Expected Just " <> item <> ", got Nothing"
    Just a -> pure a

expectRight :: (HasCallStack, MonadIO m, Show e) => String -> Either e a -> m a
expectRight item = \case
    Left e ->
        expectationFailure
            $ "Expected Right "
            <> item
            <> ", got Left: "
            <> show e
    Right a -> pure a

expectationFailure :: (HasCallStack, MonadIO m) => String -> m a
expectationFailure msg = HSpec.expectationFailure msg >> error "never here"
