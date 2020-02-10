{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Customers
    ( getAdminCustomersR
    )
where

import Restyled.Prelude.Esqueleto

import qualified Data.Function as Function
import Restyled.Foundation
import Restyled.GitHubOrg
import Restyled.Models
import Restyled.Settings
import Restyled.Yesod

data Customer = Customer
    { cAccount :: Maybe CustomerAccount
    , cUser :: Maybe CustomerUser
    }

data CustomerAccount = CustomerAccount
    { caMarketplaceAccountLogin :: GitHubUserName
    , caMarketplacePlanName :: Text
    }

data CustomerUser = CustomerUser
    { cuGitHubLogin :: GitHubUserName
    , cuEmail :: Text
    , cuGitHubOrgs :: [GitHubOrg]
    }

instance Eq CustomerUser where
    (==) = (==) `Function.on` cuGitHubLogin

userInAccount :: CustomerUser -> CustomerAccount -> Bool
userInAccount CustomerUser {..} CustomerAccount {..} =
    caMarketplaceAccountLogin `elem` logins
    where logins = cuGitHubLogin : map githubOrgLogin cuGitHubOrgs

getAdminCustomersR :: Handler Html
getAdminCustomersR = do
    customers <-
        runDB
        $ alignAccountsWithUsers
        <$> fetchCustomerAccounts
        <*> fetchCustomerUsers

    adminLayout $ do
        setTitle "Restyled Admin / Customers"
        $(widgetFile "admin/customers")

alignAccountsWithUsers :: [CustomerAccount] -> [CustomerUser] -> [Customer]
alignAccountsWithUsers accounts users = found <> orphans
  where
    found = concatMap
        (\account ->
            accountCustomers account $ filter (`userInAccount` account) users
        )
        accounts

    orphans = mapMaybe (orphanCustomer found) users

accountCustomers :: CustomerAccount -> [CustomerUser] -> [Customer]
accountCustomers account [] =
    [Customer { cAccount = Just account, cUser = Nothing }]
accountCustomers account users = map (Customer (Just account) . Just) users

orphanCustomer :: [Customer] -> CustomerUser -> Maybe Customer
orphanCustomer found user = do
    guard $ Just user `notElem` map cUser found
    pure Customer { cAccount = Nothing, cUser = Just user }

fetchCustomerAccounts :: MonadIO m => SqlPersistT m [CustomerAccount]
fetchCustomerAccounts =
    selectMap (uncurry CustomerAccount . unValue2)
        $ from
        $ \(accounts `InnerJoin` plans) -> do
              on
                  $ plans
                  ^. persistIdField
                  ==. accounts
                  ^. MarketplaceAccountMarketplacePlan
              pure
                  ( accounts ^. MarketplaceAccountGithubLogin
                  , plans ^. MarketplacePlanName
                  )

fetchCustomerUsers :: SqlPersistT Handler [CustomerUser]
fetchCustomerUsers = do
    users <- select $ from $ \users -> do
        where_ $ not_ $ isNothing $ users ^. UserEmail
        where_ $ not_ $ isNothing $ users ^. UserGithubUsername
        orderBy [asc $ users ^. UserGithubUsername]
        pure users

    lift $ catMaybes <$> traverse fetchCustomerUser users

fetchCustomerUser :: Entity User -> Handler (Maybe CustomerUser)
fetchCustomerUser (Entity _ user) =
    runMaybeT
        $ CustomerUser
        <$> hoistMaybe (userGithubUsername user)
        <*> hoistMaybe (userEmail user)
        <*> lift (requestUserOrgs user)
