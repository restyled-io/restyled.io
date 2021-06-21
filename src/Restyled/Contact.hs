{-# LANGUAGE TupleSections #-}

module Restyled.Contact
    ( Contact(..)
    , contactFromUser
    , contactsFromMarketplaceAccount
    , fetchAllUserContacts
    , fetchMarketplaceAccountContacts
    ) where

import Restyled.Prelude

import Control.Monad.Logger
import Restyled.Cache
import Restyled.GitHubOrg
import Restyled.Models
import Restyled.Settings (HasSettings)
import qualified RIO.HashMap as HashMap

data Contact = Contact
    { contactType :: Text
    , contactEmail :: Text
    , contactGithubUsername :: Maybe GitHubUserName
    }

contactFromUser :: User -> Maybe Contact
contactFromUser user = do
    email <- userEmail user
    pure $ Contact
        { contactType = "User"
        , contactEmail = email
        , contactGithubUsername = userGithubUsername user
        }

contactsFromMarketplaceAccount :: MarketplaceAccount -> [Contact]
contactsFromMarketplaceAccount MarketplaceAccount {..} = catMaybes
    [ contact "Account" <$> marketplaceAccountEmail
    , contact "Billing" <$> marketplaceAccountBillingEmail
    ]
  where
    contact t e = Contact
        { contactType = t
        , contactEmail = e
        , contactGithubUsername = Nothing
        }

fetchAllUserContacts :: MonadIO m => SqlPersistT m [Contact]
fetchAllUserContacts = do
    users <- selectList
        [UserEmail !=. Nothing, UserGithubUsername !=. Nothing]
        [Asc UserEmail]
    pure $ mapMaybe (contactFromUser . entityVal) users

fetchMarketplaceAccountContacts
    :: ( MonadUnliftIO m
       , MonadLogger m
       , MonadCache m
       , MonadReader env m
       , HasSqlPool env
       , HasSettings env
       )
    => MarketplaceAccount
    -> m [Contact]

fetchMarketplaceAccountContacts account@MarketplaceAccount {..} = do
    contacts <- runDB fetchAllUserContacts
    contactsByOrg <- fetchContactsToByOrg contacts
    let
        accountOrgContacts =
            HashMap.lookupDefault [] marketplaceAccountGithubLogin contactsByOrg
    pure $ contactsFromMarketplaceAccount account <> accountOrgContacts

fetchContactsToByOrg
    :: ( MonadIO m
       , MonadLogger m
       , MonadCache m
       , MonadReader env m
       , HasSettings env
       )
    => [Contact]
    -> m (HashMap GitHubUserName [Contact])
fetchContactsToByOrg contacts = do
    results <- for contacts $ \contact -> do
        orgs <- case contactGithubUsername contact of
            Nothing -> pure []
            Just login -> requestUserNameOrgs login

        pure $ map ((, [contact]) . githubOrgLogin) orgs
    pure $ HashMap.fromListWith (<>) $ concat results
