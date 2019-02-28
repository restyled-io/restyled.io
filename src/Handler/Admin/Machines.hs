{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Admin.Machines
    ( getAdminMachinesR
    , getAdminMachinesNewR
    , postAdminMachinesR
    , deleteAdminMachineR
    ) where

import Import

machineForm :: Form RestyleMachine
machineForm =
    renderDivs
        $ RestyleMachine
        <$> areq textField "Name" Nothing
        <*> areq checkBoxField "Enabled" (Just True)
        <*> areq textField "Host" Nothing
        <*> (unTextarea <$> areq textareaField "Certificate Authority" Nothing)
        <*> (unTextarea <$> areq textareaField "Certificate" Nothing)
        <*> (unTextarea <$> areq textareaField "Private Key" Nothing)

getAdminMachinesR :: Handler Html
getAdminMachinesR = do
    machines <- runDB
        $ selectList [] [Desc RestyleMachineEnabled, Asc RestyleMachineName]
    adminLayout $ do
        setTitle "Restyled Admin / Machines"
        $(widgetFile "admin/machines")

getAdminMachinesNewR :: Handler Html
getAdminMachinesNewR = do
    (widget, enctype) <- generateFormPost machineForm

    adminLayout $ do
        setTitle "Restyled Amin / New Machine"
        $(widgetFile "admin/machines/new")

postAdminMachinesR :: Handler Html
postAdminMachinesR = do
    ((result, widget), enctype) <- runFormPost machineForm

    case result of
        FormSuccess machine -> do
            void $ runDB $ insert machine
            setMessage "Machine created"
            redirect $ AdminP $ AdminMachinesP AdminMachinesR

        _ -> do
            setMessage "Unable to create machine"
            adminLayout $ do
                setTitle "Restyled Amin / New Machine"
                $(widgetFile "admin/machines/new")

deleteAdminMachineR :: RestyleMachineId -> Handler Html
deleteAdminMachineR machineId = do
    runDB $ do
        void $ get404 machineId
        delete machineId
    setMessage "Machine deleted"
    redirect $ AdminP $ AdminMachinesP AdminMachinesR
