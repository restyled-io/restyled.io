module Restyled.Export.Customers
    ( Customer(..)
    , fetchCustomers
    )
where

import Restyled.Prelude.Esqueleto

import Control.Monad.Logger (MonadLogger)
import qualified Data.Csv as Csv
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import Data.Semigroup (First(..), sconcat)
import Restyled.Cache
import Restyled.GitHubOrg
import Restyled.Models
import Restyled.Settings

data Customer = Customer
    { planName :: Text
    , accountLogin :: GitHubUserName
    , accountType :: Text
    , accountEmail :: Maybe Text
    }
    deriving stock Generic
    deriving anyclass (Csv.DefaultOrdered, Csv.ToNamedRecord)

fetchCustomers
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadCache m
       , MonadReader env m
       , HasSettings env
       , HasDB env
       )
    => m [Customer]
fetchCustomers = do
    accounts <-
        runDB $ selectMap unValue5 $ from $ \(accounts `InnerJoin` plans) -> do
            on
                $ plans
                ^. persistIdField
                ==. accounts
                ^. MarketplaceAccountMarketplacePlan
            pure
                ( plans ^. MarketplacePlanName
                , accounts ^. MarketplaceAccountGithubLogin
                , accounts ^. MarketplaceAccountGithubType
                , accounts ^. MarketplaceAccountEmail
                , accounts ^. MarketplaceAccountBillingEmail
                )

    emailLookup <- fetchEmailLookup

    let
        buildCustomer (planName, accountLogin, accountType, accountEmail, accountBillingEmail)
            = Customer
                { planName
                , accountLogin
                , accountType
                , accountEmail = resolveEmails
                    [ First <$> accountEmail
                    , First <$> accountBillingEmail
                    , Map.lookup accountLogin emailLookup
                    ]
                }

    pure $ map buildCustomer accounts

resolveEmails :: [Maybe (First a)] -> Maybe a
resolveEmails = fmap (getFirst . sconcat) . NE.nonEmpty . catMaybes

fetchEmailLookup
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadCache m
       , MonadReader env m
       , HasSettings env
       , HasDB env
       )
    => m (Map GitHubUserName (First Text))
fetchEmailLookup = do
    users <- runDB $ selectMap unValue2 $ from $ \users -> do
        email <- refineNotNull $ users ^. UserEmail
        login <- refineNotNull $ users ^. UserGithubUsername
        orderBy [asc $ users ^. UserGithubUsername]
        pure (email, login)

    loginEmails <- for users $ \(email, login) -> do
        orgs <- githubOrgLogin <$$> requestUserNameOrgs login
        pure $ (login, First email) : map (, First email) orgs

    pure $ Map.fromListWith (<>) $ concat loginEmails
