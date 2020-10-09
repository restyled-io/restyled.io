{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Offers
    ( getAdminOffersR
    , postAdminOffersR
    , deleteAdminOfferR
    , getAdminOfferClaimsR
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Models
import Restyled.Offers
import Restyled.Settings
import Restyled.Yesod

createOfferForm :: Form CreateOffer
createOfferForm =
    renderDivs
        $ CreateOffer
        <$> areq textField "Name" Nothing
        <*> (unTextarea <$> areq textareaField "Details" Nothing)
        <*> areq textField "URL where this Offer can be purchased" Nothing
        <*> areq intField "Number of claims to generate" (Just 100)
        <*> (entityKey <$$> aopt selectPlan "Use an existing Plan" Nothing)
        <*> aopt intField "Or create with with this allowance" Nothing
  where
    selectPlan :: Field Handler (Entity MarketplacePlan)
    selectPlan = selectField $ optionsPersist
        [MarketplacePlanGithubId ==. Nothing]
        []
        marketplacePlanName

getAdminOffersR :: Handler Html
getAdminOffersR = do
    (widget, enctype) <- generateFormPost createOfferForm
    offers <- runDB $ selectList [] [Asc OfferName]
    adminLayout $ do
        setTitle "Offers"
        $(widgetFile "admin/offers")

postAdminOffersR :: Handler Html
postAdminOffersR = do
    ((result, widget), enctype) <- runFormPost createOfferForm

    case result of
        FormSuccess co -> do
            runDB $ createOffer co
            setMessage "Offer created"
            redirect $ AdminP $ AdminOffersP AdminOffersR
        _ -> do
            setMessage "Form errors"
            offers <- runDB $ selectList [] [Asc OfferName]
            adminLayout $ do
                setTitle "Offers"
                $(widgetFile "admin/offers")

deleteAdminOfferR :: OfferId -> Handler Html
deleteAdminOfferR offerId = do
    void $ runDB $ do
        deleteWhere [OfferClaimOffer ==. offerId]
        delete offerId
    setMessage "Offer deleted"
    redirect $ AdminP $ AdminOffersP AdminOffersR

getAdminOfferClaimsR :: OfferId -> Handler TypedContent
getAdminOfferClaimsR offerId = do
    claims <- runDB $ selectList [OfferClaimOffer ==. offerId] []
    addContentDispositionFileName "claims.csv"
    sendResponseCSV claims
