{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Admin.Users
    ( getAdminUsersR
    , deleteAdminUserR
    ) where

import Import

getAdminUsersR :: Handler Html
getAdminUsersR = do
    users <- runDB $ selectList [] [Asc UserEmail]

    adminLayout $ do
        setTitle "Restyled Admin / Users"
        $(widgetFile "admin/users")

deleteAdminUserR :: UserId -> Handler ()
deleteAdminUserR userId = do
    runDB $ do
        void $ get404 userId
        delete userId
    setMessage "User deleted"
    redirect $ AdminP $ AdminUsersP AdminUsersR
