{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Handler.Admin.Plans
    ( getAdminPlansR
    , getAdminPlansNewR
    , postAdminPlansR
    , deleteAdminPlanR
    ) where

import Import

import Formatting (format)
import Formatting.Time (diff)
import GitHub.Data (mkName)

planForm :: Form Plan
planForm =
    renderDivs
        $ Plan
        <$> areq (selectField optionsEnum) "Type" Nothing
        <*> (mkName Proxy <$> areq textField "Owner" Nothing)
        <*> (mkName Proxy <$> areq textField "Repo" Nothing)
        <*> (mkUTC <$$> aopt dayField "Active" Nothing)
        <*> (mkUTC <$$> aopt dayField "Expires" Nothing)
        <*> (unTextarea <$> areq textareaField "Message" Nothing)
  where
    mkUTC :: Day -> UTCTime
    mkUTC = flip UTCTime 0

getAdminPlansR :: Handler Html
getAdminPlansR = do
    now <- liftIO getCurrentTime
    plans <- runDB $ selectList [] [Asc PlanOwner, Asc PlanRepo, Desc PlanId]
    adminLayout $ do
        setTitle "Restyled Admin / Plans"
        $(widgetFile "admin/plans")

getAdminPlansNewR :: Handler Html
getAdminPlansNewR = do
    (widget, enctype) <- generateFormPost planForm

    adminLayout $ do
        setTitle "Restyled Amin / New Plan"
        $(widgetFile "admin/plans/new")

postAdminPlansR :: Handler Html
postAdminPlansR = do
    ((result, widget), enctype) <- runFormPost planForm

    case result of
        FormSuccess plan -> do
            void $ runDB $ insert plan
            setMessage "Plan created"
            redirect $ AdminP $ AdminPlansP AdminPlansR

        _ -> do
            setMessage "Unable to create plan"
            adminLayout $ do
                setTitle "Restyled Amin / New Plan"
                $(widgetFile "admin/plans/new")

deleteAdminPlanR :: PlanId -> Handler Html
deleteAdminPlanR planId = do
    runDB $ do
        void $ get404 planId
        delete planId
    setMessage "Plan deleted"
    redirect $ AdminP $ AdminPlansP AdminPlansR
