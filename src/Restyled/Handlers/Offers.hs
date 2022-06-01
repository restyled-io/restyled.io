{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Restyled.Handlers.Offers
    ( getOfferR
    , postOfferClaimR
    ) where

import Restyled.Prelude

import Restyled.DB
import Restyled.Foundation
import Restyled.Models
import Restyled.Offers
import Restyled.Settings
import Restyled.Yesod

-- brittany-disable-next-binding

claimDetailsForm :: Maybe User -> Form ClaimDetails
claimDetailsForm mUser = renderDivs $ ClaimDetails
    <$> (mkUserName <$>
        areq textField "User or Organization where the App will be installed:" mUserLoginText)
    <*> aopt textField "Email address (optional, never shared):" (Just mEmail)
    <*> areq textField "Claim Code:" Nothing
 where
    mUserLogin = userGithubUsername =<< mUser
    mUserLoginText = toPathPart <$> mUserLogin
    mEmail = userEmail =<< mUser

getOfferR :: OfferId -> Handler Html
getOfferR offerId = do
    mUser <- entityVal <$$> maybeAuth
    Offer {..} <- runDB $ get404 offerId
    (widget, enctype) <- generateFormPost $ claimDetailsForm mUser

    defaultLayout $ do
        setTitle "Redeem Offer"
        $(widgetFile "offers/offer")

postOfferClaimR :: OfferId -> Handler Html
postOfferClaimR offerId = do
    mUser <- entityVal <$$> maybeAuth
    Offer {..} <- runDB $ get404 offerId
    ((result, widget), enctype) <- runFormPost $ claimDetailsForm mUser

    case result of
        FormSuccess details -> do
            eResult <- runDB $ runExceptT $ claimOffer offerId details
            case eResult of
                Left er -> setMessage $ toHtml er
                Right _ -> setMessage "Offer claimed!"
            redirect $ OffersP $ OfferP offerId OfferR
        _ -> do
            setMessage "Invalid Claim details"
            defaultLayout $ do
                setTitle "Redeem Offer"
                $(widgetFile "offers/offer")
