module Restyled.Admin.RepoSearch
    ( SearchResults(..)
    , noResults
    , searchRepos
    )
where

import Restyled.Prelude

import Restyled.Foundation
import Restyled.Models

data SearchResults
    = SearchResults
    { srRepos :: [Entity Repo]
    , srTotal :: Int
    }
    deriving Generic

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

    pure SearchResults {srRepos = repos, srTotal = total}

searchFilters :: Text -> [Filter Repo]
searchFilters q = [RepoOwner `ilike` q] ||. [RepoName `ilike` q]

ilike
    :: (IsString a, PersistField a)
    => EntityField record a
    -> Text
    -> Filter record
ilike field value = Filter
    field
    (Left $ fromString $ unpack $ "%" <> value <> "%")
    (BackendSpecificFilter "ILIKE")
