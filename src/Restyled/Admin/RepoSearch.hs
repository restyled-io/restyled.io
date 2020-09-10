module Restyled.Admin.RepoSearch
    ( SearchResults(..)
    , noResults
    , searchRepos
    )
where

import Restyled.Prelude

import qualified Data.Text as T
import Restyled.Foundation
import Restyled.Models

data SearchResults
    = SearchResults
    { srRepos :: [Entity Repo]
    , srTotal :: Int
    }
    deriving stock Generic

instance ToJSON SearchResults where
    toJSON = genericToJSON $ aesonPrefix camelCase
    toEncoding = genericToEncoding $ aesonPrefix camelCase

-- | Empty @'SearchResults'@
noResults :: SearchResults
noResults = SearchResults [] 0

-- | Search for @'Repo'@s by Owner or Name
--
-- This is just @owner|name ILIKE %{query}%@
--
searchRepos :: Int -> Text -> Handler SearchResults
searchRepos limit q = runDB $ do
    repos <- selectList (searchFilters q) [LimitTo limit]
    total <- if length repos == limit
        then count $ searchFilters q
        else pure $ length repos

    pure SearchResults { srRepos = repos, srTotal = total }

searchFilters :: Text -> [Filter Repo]
searchFilters q = case T.breakOn "/" q of
    (owner, name) | not (T.null name) ->
        [RepoOwner ==. mkOwnerName owner, RepoName `ilike` T.drop 1 name]
    _ -> [RepoOwner `ilike` q] ||. [RepoName `ilike` q]

ilike
    :: (IsString a, PersistField a)
    => EntityField record a
    -> Text
    -> Filter record
ilike field value = Filter
    field
    (FilterValue $ fromString $ unpack $ "%" <> value <> "%")
    (BackendSpecificFilter "ILIKE")
