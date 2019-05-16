module Backend.Marketplace
    ( synchronizeMarketplacePlans
    , MarketplacePlanAllows(..)
    , MarketplacePlanLimitation(..)
    , marketplacePlanAllows
    , whenMarketplacePlanForbids
    , isPrivateRepoPlan
    )
where

import Backend.Import

import Network.HTTP.Client (parseRequest)
import SVCS.GitHub.ApiClient

data GitHubMarketplacePlan = GitHubMarketplacePlan
    { ghmpId :: Int
    , ghmpName :: Text
    , ghmpDescription :: Text
    }
    deriving (Show, Generic)

instance FromJSON GitHubMarketplacePlan where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

data GitHubAccount = GitHubAccount
    { ghaId :: GitHubUserId
    , ghaLogin :: GitHubUserName
    }
    deriving (Show, Generic)

instance FromJSON GitHubAccount where
    parseJSON = genericParseJSON $ aesonPrefix snakeCase

synchronizeMarketplacePlans
    :: (HasLogFunc env, HasSettings env, HasDB env) => RIO env a
synchronizeMarketplacePlans = do
    handleAny (logWarn . displayShow) runSynchronize
    liftIO $ threadDelay $ 5 * 60 * 1000000
    synchronizeMarketplacePlans

runSynchronize :: (HasLogFunc env, HasSettings env, HasDB env) => RIO env ()
runSynchronize = do
    logInfo "Synchronizing GitHub Marketplace data"
    plans <- getGitHub $ marketplaceListingPath <> "/plans"
    synchronizedAccountIds <- for plans $ \plan -> do
        logDebug $ "Plan: " <> displayShow plan
        planId <- runDB $ entityKey <$> upsert
            MarketplacePlan
                { marketplacePlanGithubId = ghmpId plan
                , marketplacePlanName = ghmpName plan
                , marketplacePlanDescription = ghmpDescription plan
                }
            [ MarketplacePlanName =. ghmpName plan
            , MarketplacePlanDescription =. ghmpDescription plan
            ]

        accounts <-
            getGitHub
            $ marketplaceListingPath
            <> "/plans/"
            <> toPathPiece (ghmpId plan)
            <> "/accounts"

        for accounts $ \account -> do
            logDebug $ "Account: " <> displayShow account
            runDB $ entityKey <$> upsert
                MarketplaceAccount
                    { marketplaceAccountGithubId = ghaId account
                    , marketplaceAccountGithubLogin = ghaLogin account
                    , marketplaceAccountMarketplacePlan = planId
                    }
                [MarketplaceAccountMarketplacePlan =. planId]

    logInfo "GitHub Marketplace data synchronized"
    runDB $ deleteUnsynchronized $ mconcat synchronizedAccountIds

deleteUnsynchronized :: MonadIO m => [MarketplaceAccountId] -> SqlPersistT m ()
deleteUnsynchronized synchronizedAccountIds = do
    planId <- entityKey <$> fetchDiscountMarketplacePlan

    deleteWhere
        [ MarketplaceAccountId /<-. synchronizedAccountIds
        , MarketplaceAccountMarketplacePlan !=. planId
        ]

fetchDiscountMarketplacePlan
    :: MonadIO m => SqlPersistT m (Entity MarketplacePlan)
fetchDiscountMarketplacePlan =
    assertJust "Discount Plan must exist"
        =<< selectFirst [MarketplacePlanGithubId ==. 0] []

getGitHub :: (FromJSON a, HasSettings env) => Text -> RIO env a
getGitHub path = do
    AppSettings {..} <- view settingsL

    liftIO $ do
        request <- parseRequest $ unpack $ "GET https://api.github.com" <> path
        requestJWT appGitHubAppId appGitHubAppKey request

data MarketplacePlanAllows
    = MarketplacePlanAllows
    | MarketplacePlanForbids MarketplacePlanLimitation

data MarketplacePlanLimitation
    = MarketplacePlanNotFound
    | MarketplacePlanPublicOnly

-- | Current, naive @'MarketplacePlan'@ limitations
marketplacePlanAllows
    :: MonadIO m => Entity Repo -> SqlPersistT m MarketplacePlanAllows
marketplacePlanAllows (Entity _ Repo {..}) = do
    mPlan <- fetchMarketplacePlanByLogin $ ownerToUserName repoOwner

    pure $ case (repoIsPrivate, mPlan) of
        (False, _) -> MarketplacePlanAllows
        (True, Nothing) -> MarketplacePlanForbids MarketplacePlanNotFound
        (True, Just plan)
            | isPrivateRepoPlan plan -> MarketplacePlanAllows
            | otherwise -> MarketplacePlanForbids MarketplacePlanPublicOnly

whenMarketplacePlanForbids
    :: Applicative f
    => MarketplacePlanAllows
    -> (MarketplacePlanLimitation -> f ())
    -> f ()
whenMarketplacePlanForbids MarketplacePlanAllows _ = pure ()
whenMarketplacePlanForbids (MarketplacePlanForbids limitation) f = f limitation

isPrivateRepoPlan :: MarketplacePlan -> Bool
isPrivateRepoPlan MarketplacePlan {..} =
    marketplacePlanGithubId `elem` privateRepoPlanGitHubIds

privateRepoPlanGitHubIds :: [Int]
privateRepoPlanGitHubIds =
    [ 0 -- Manually-managed "Friends & Family" plan
    , 2178 -- Temporary "Early Adopter" plan
    ]
