module Restyled.Handlers.Marketplace
    ( postRepoMarketplaceClaimR
    , deleteRepoMarketplaceClaimR
    )
where

import Restyled.Prelude

import Restyled.ApiError
import Restyled.Foundation
import Restyled.Models
import Restyled.PrivateRepoAllowance
import Restyled.Yesod

postRepoMarketplaceClaimR :: OwnerName -> RepoName -> Handler TypedContent
postRepoMarketplaceClaimR owner name = do
    mEnabled <- runDB $ do
        user <- requireAuth
        repo <- getBy404 $ UniqueRepo GitHubSVCS owner name
        enableMarketplaceRepoForUser repo user

    case mEnabled of
        Nothing -> notFound
        Just PrivateRepoEnabled -> do
            setMessage "Private repository enabled"
            redirectOr $ sendResponseStatus status201 ()
        Just PrivateRepoNotAllowed ->
            badRequest "Plan does not support private repositories"
        Just PrivateRepoLimited ->
            badRequest "Plan has reached private repositories limit"

deleteRepoMarketplaceClaimR :: OwnerName -> RepoName -> Handler TypedContent
deleteRepoMarketplaceClaimR owner name = do
    runDB $ do
        user <- requireAuth
        repo <- getBy404 $ UniqueRepo GitHubSVCS owner name
        disableMarketplaceRepoForUser repo user

    setMessage "Repository disabled"
    redirectOr $ sendResponseStatus status204 ()

badRequest :: Text -> Handler TypedContent
badRequest msg = do
    setMessage $ toHtml msg
    redirectOr $ sendApiError $ ApiErrorBadRequest msg

redirectOr :: Handler Value -> Handler TypedContent
redirectOr val = selectRep $ do
    provideRep @_ @Html $ redirect ProfileR
    provideRep val
