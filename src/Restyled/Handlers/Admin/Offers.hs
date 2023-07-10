{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}

module Restyled.Handlers.Admin.Offers
  ( getAdminOffersR
  , postAdminOffersR
  , deleteAdminOfferR
  , getAdminOfferClaimsR
  , postAdminOfferClaimsR
  ) where

import Restyled.Prelude.Esqueleto hiding (Value)

import Data.Aeson.Lens
import qualified Data.Text.Lazy.Encoding as TL
import qualified Database.Persist as P
import Lens.Micro ((^?))
import Restyled.DB
import Restyled.Foundation
import Restyled.Models
import Restyled.Offers
import qualified Restyled.Prelude.Esqueleto as E
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
  selectPlan =
    selectField
      $ optionsPersist
        [MarketplacePlanGithubId P.==. Nothing]
        []
        marketplacePlanName

getAdminOffersR :: Handler Html
getAdminOffersR = do
  (widget, enctype) <- generateFormPost createOfferForm
  offers <- runDB fetchOffersWithCodes
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
      offers <- runDB fetchOffersWithCodes
      adminLayout $ do
        setTitle "Offers"
        $(widgetFile "admin/offers")

deleteAdminOfferR :: OfferId -> Handler Html
deleteAdminOfferR offerId = do
  void $ runDB $ do
    P.deleteWhere [OfferClaimOffer P.==. offerId]
    P.delete offerId
  setMessage "Offer deleted"
  redirect $ AdminP $ AdminOffersP AdminOffersR

getAdminOfferClaimsR :: OfferId -> Handler TypedContent
getAdminOfferClaimsR offerId = do
  claims <- runDB $ P.selectList [OfferClaimOffer P.==. offerId] []
  addContentDispositionFileName "claims.csv"
  sendResponseCSV claims

postAdminOfferClaimsR :: OfferId -> Handler TypedContent
postAdminOfferClaimsR offerId = do
  let
    defaultN :: Int
    defaultN = 10

  selectRep $ do
    provideRep @_ @Value $ do
      body <- requireCheckJsonBody @_ @Value
      let n = body ^? key "number" . _Integer
      runDB $ addClaimCodes offerId $ maybe defaultN fromIntegral n
      sendStatusJSON status201 $ object ["claims" .= n]
    provideRep @_ @Html $ do
      n <- fromMaybe defaultN <$> runInputPost (iopt intField "number")
      runDB $ addClaimCodes offerId n
      setMessage $ toHtml $ show @Text n <> " claim codes generated"
      redirect $ AdminP $ AdminOffersP AdminOffersR

data ClaimCode = ClaimCode
  { code :: Text
  , claimed :: Bool
  }
  deriving stock (Generic)
  deriving anyclass (ToJSON)

claimCode :: Text -> Maybe GitHubUserName -> ClaimCode
claimCode code mAt = ClaimCode {code, claimed = isJust mAt}

fetchOffersWithCodes
  :: MonadIO m => SqlPersistT m [(Entity Offer, [ClaimCode])]
fetchOffersWithCodes =
  selectMap convert $ from $ \(offers `InnerJoin` codes) -> do
    on $ codes ^. OfferClaimOffer ==. offers ^. persistIdField
    groupBy $ offers ^. persistIdField
    pure
      ( offers
      , aggregate $ codes ^. OfferClaimCode
      , aggregate $ codes ^. OfferClaimClaimedFor
      )
 where
  convert
    :: (a, E.Value [Text], E.Value [Maybe GitHubUserName])
    -> (a, [ClaimCode])
  convert (a, ts, mbs) = (a, zipWith claimCode (unValue ts) (unValue mbs))
