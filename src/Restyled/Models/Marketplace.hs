module Restyled.Models.Marketplace
    ( fetchMarketplacePlanAccounts
    ) where

import Restyled.Prelude.Esqueleto

import qualified Data.List.NonEmpty as NE
import Restyled.Models.DB

fetchMarketplacePlanAccounts
    :: MonadIO m
    => SqlPersistT m [(Entity MarketplacePlan, [Entity MarketplaceAccount])]
fetchMarketplacePlanAccounts =
    fmap groupJoined $ select $ from $ \(plans `LeftOuterJoin` accounts) -> do
        on $ (accounts ?. MarketplaceAccountMarketplacePlan) ==. just
            (plans ^. persistIdField)

        -- Omit retired plans unless there are legacy accounts present
        where_ $ not_ $ (plans ^. MarketplacePlanRetired) &&. isNothing
            (accounts ?. persistIdField)

        orderBy
            [ asc $ plans ^. MarketplacePlanRetired
            , desc $ plans ^. MarketplacePlanMonthlyRevenue
            , asc $ plans ^. MarketplacePlanName
            , asc $ accounts ?. MarketplaceAccountGithubId
            ]

        pure (plans, accounts)
  where
    groupJoined :: Ord (Key a) => [(Entity a, Maybe b)] -> [(Entity a, [b])]
    groupJoined = map toPair . NE.groupAllWith (entityKey . fst)

    toPair :: NonEmpty (a, Maybe b) -> (a, [b])
    toPair = fst . NE.head &&& mapMaybe snd . NE.toList
