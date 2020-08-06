{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Machines
    ( getAdminMachinesR
    , getAdminMachinesNewR
    , postAdminMachinesR
    , getAdminMachineR
    , patchAdminMachineR
    , deleteAdminMachineR
    , getAdminMachineInfoR
    , getAdminMachinePruneR
    )
where

import Restyled.Prelude

import Restyled.Admin.CreateMachine
import Restyled.Backend.Reconcile (reconcileMachine)
import Restyled.Backend.RestyleMachine
import Restyled.Foundation
import Restyled.Models
import Restyled.Settings
import Restyled.WebSockets
import Restyled.Yesod

machineForm :: Form (Handler RestyleMachine)
machineForm =
    renderDivs
        $ createMachineGetRestyleMachine
        <$> areq jsonField "Machine JSON" { fsAttrs = attrs } Nothing
    where attrs = [("placeholder", createMachinePlaceholder), ("rows", "30")]

getAdminMachinesR :: Handler TypedContent
getAdminMachinesR = do
    machines <- runDB
        $ selectList [] [Desc RestyleMachineEnabled, Asc RestyleMachineName]

    selectRep $ do
        provideRep @_ @Html $ adminLayout $ do
            setTitle "Restyled Admin / Machines"
            $(widgetFile "admin/machines")
        provideRep @_ @Value $ pure $ toJSON machines

getAdminMachinesNewR :: Handler Html
getAdminMachinesNewR = do
    (widget, enctype) <- generateFormPost machineForm

    adminLayout $ do
        setTitle "Restyled Admin / New Machine"
        $(widgetFile "admin/machines/new")

postAdminMachinesR :: Handler TypedContent
postAdminMachinesR = selectRep $ do
    provideRep @_ @Html $ do
        ((result, widget), enctype) <- runFormPost machineForm

        case result of
            FormSuccess getMachine -> do
                machine <- getMachine
                void $ runDB $ insert machine
                setMessage "Machine created"
                redirect $ AdminP $ AdminMachinesP AdminMachinesR

            _ -> do
                setMessage "Unable to create machine"
                adminLayout $ do
                    setTitle "Restyled Admin / New Machine"
                    $(widgetFile "admin/machines/new")

    provideRep @_ @Value $ do
        body <- requireCheckJsonBody
        machine <- createMachineGetRestyleMachine body
        machineE <- runDB $ insertEntity machine
        sendStatusJSON status201 machineE

getAdminMachineR :: RestyleMachineId -> Handler Value
getAdminMachineR machineId = runDB $ toJSON <$> getEntity404 machineId

newtype AdminMachinePatch = AdminMachinePatch
    { enabled :: Maybe Bool
    }
    deriving stock Generic
    deriving anyclass FromJSON

patchAdminMachineR :: RestyleMachineId -> Handler TypedContent
patchAdminMachineR machineId = selectRep $ do
    provideRep @_ @Html $ do
        enabled <- runInputPost $ ireq boolField "enabled"
        runDB $ do
            void $ get404 machineId
            update machineId [RestyleMachineEnabled =. enabled]
        setMessage $ "Machine " <> if enabled then "enabled" else "disabled"
        redirect $ AdminP $ AdminMachinesP AdminMachinesR

    provideRep @_ @Value $ do
        AdminMachinePatch {..} <- requireCheckJsonBody
        runDB $ do
            update machineId
                $ catMaybes [(RestyleMachineEnabled =.) <$> enabled]
            toJSON <$> getEntity404 machineId

deleteAdminMachineR :: RestyleMachineId -> Handler TypedContent
deleteAdminMachineR machineId = do
    machine <- runDB $ getEntity404 machineId

    -- Attempt to reconcile the machine. Best effort and capped at just under
    -- the Heroku request timeout. We don't want to hold up deletes on this.
    result <-
        timeout (28 * 1000000)
        $ handleAny (logErrorN . tshow)
        $ void
        $ (`withRestyleMachineEnv` reconcileMachine)
        $ entityVal machine
    logInfoN $ "Reconcile result: " <> tshow result

    runDB $ deleteRestyleMachine machine

    selectRep $ do
        provideRep @_ @Value $ sendResponseNoContent
        provideRep @_ @Html $ do
            setMessage "Machine deleted"
            redirect $ AdminP $ AdminMachinesP AdminMachinesR

getAdminMachineInfoR :: RestyleMachineId -> Handler ()
getAdminMachineInfoR machineId = do
    machine <- runDB $ get404 machineId

    withAsyncWebSockets_ $ \write -> do
        write "Fetching information..."
        withRestyleMachineEnv machine $ proc "docker" ["info"] $ followProcess
            (write . pack . ("[stdout]: " <>))
            (write . pack . ("[stderr]: " <>))

-- NB must be GET for WebSockets response
getAdminMachinePruneR :: RestyleMachineId -> Handler ()
getAdminMachinePruneR machineId = do
    machine <- runDB $ get404 machineId

    withAsyncWebSockets_ $ \write -> do
        write "Pruning host..."
        ec <-
            withRestyleMachineEnv machine
            $ proc "docker" ["system", "prune", "--all", "--force"]
            $ followProcess
                  (write . pack . ("[stdout]: " <>))
                  (write . pack . ("[stderr]: " <>))
        write $ tshow ec
