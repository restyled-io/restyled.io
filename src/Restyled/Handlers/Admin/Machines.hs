{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Machines
    ( getAdminMachinesR
    , getAdminMachinesNewR
    , postAdminMachinesR
    , patchAdminMachineR
    , deleteAdminMachineR
    , getAdminMachineInfoR
    , postAdminMachinePruneR
    )
where

import Restyled.Prelude

import qualified Data.Text.Lazy.Encoding as TL
import Restyled.Backend.RestyleMachine
import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.Yesod

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
        setTitle "Restyled Admin / New Machine"
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
                setTitle "Restyled Admin / New Machine"
                $(widgetFile "admin/machines/new")

patchAdminMachineR :: RestyleMachineId -> Handler Html
patchAdminMachineR machineId = do
    enabled <- runInputPost $ ireq boolField "enabled"
    runDB $ do
        void $ get404 machineId
        update machineId [RestyleMachineEnabled =. enabled]
    setMessage $ "Machine " <> if enabled then "enabled" else "disabled"
    redirect $ AdminP $ AdminMachinesP AdminMachinesR

deleteAdminMachineR :: RestyleMachineId -> Handler Html
deleteAdminMachineR machineId = do
    runDB $ do
        void $ get404 machineId
        delete machineId
    setMessage "Machine deleted"
    redirect $ AdminP $ AdminMachinesP AdminMachinesR

getAdminMachineInfoR :: RestyleMachineId -> Handler Html
getAdminMachineInfoR machineId = do
    machine <- runDB $ get404 machineId
    (ec', out', err') <- withRestyleMachineEnv machine
        $ proc "docker" ["info"] readProcess

    let out = TL.decodeUtf8 out'
        err = TL.decodeUtf8 err'
        ec = case ec' of
            ExitSuccess -> "0"
            ExitFailure n -> show n

    adminLayout $ do
        setTitle "Restyled Admin / Machine Info"
        $(widgetFile "admin/machines/info")

postAdminMachinePruneR :: RestyleMachineId -> Handler Html
postAdminMachinePruneR machineId = do
    machine <- runDB $ get404 machineId
    (ec', out', err') <- withRestyleMachineEnv machine
        $ proc "docker" ["system", "prune", "--all", "--force"] readProcess

    let out = TL.decodeUtf8 out'
        err = TL.decodeUtf8 err'
        ec = case ec' of
            ExitSuccess -> "0"
            ExitFailure n -> show n

    adminLayout $ do
        setTitle "Restyled Admin / Machine Prune"
        $(widgetFile "admin/machines/prune")
