{-# LANGUAGE NamedFieldPuns #-}
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
import Restyled.Backend.Reconcile
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
  where
    attrs :: [(Text, Text)]
    attrs = [("placeholder", createMachinePlaceholder), ("rows", "30")]

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

data AdminMachinePatch = AdminMachinePatch
    { enabled :: Maybe Bool
    , reconciling :: Maybe Bool
    }
    deriving stock Generic
    deriving anyclass FromJSON

patchAdminMachine
    :: RestyleMachineId -> AdminMachinePatch -> Handler (Entity RestyleMachine)
patchAdminMachine machineId AdminMachinePatch { enabled, reconciling } = do
    machine <- runDB $ do
        -- Don't update reconciling here, let the reconcile routine handle that
        -- itself, in a race-free way.
        let updates = catMaybes [(RestyleMachineEnabled =.) <$> enabled]
        unless (null updates) $ update machineId updates
        getEntity404 machineId
    machine <$ when
        (reconciling == Just True)
        (safelyReconcile 10 $ Just [machine])

patchAdminMachineR :: RestyleMachineId -> Handler TypedContent
patchAdminMachineR machineId = selectRep $ do
    provideRep @_ @Html $ do
        body <-
            runInputPost
            $ AdminMachinePatch
            <$> iopt boolField "enabled"
            <*> iopt boolField "reconciling"
        void $ patchAdminMachine machineId body
        setMessage "Machine updated"
        redirect $ AdminP $ AdminMachinesP AdminMachinesR

    provideRep @_ @Value $ do
        body <- requireCheckJsonBody
        toJSON <$> patchAdminMachine machineId body

deleteAdminMachineR :: RestyleMachineId -> Handler TypedContent
deleteAdminMachineR machineId = do
    machine <- runDB $ getEntity404 machineId

    safelyReconcile 10 $ Just [machine]
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
