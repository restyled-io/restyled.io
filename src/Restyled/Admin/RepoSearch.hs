module Restyled.Admin.RepoSearch
    ( SearchResults(..)
    , searchRepos
    ) where

import Restyled.Prelude

import qualified Data.Text as T
import Restyled.Api.Repo (ApiRepo, apiRepo)
import Restyled.DB
import Restyled.Foundation
import Restyled.Models
import Restyled.Paginate
import Restyled.Settings

newtype SearchResults = SearchResults
    { srPage :: Page ApiRepo
    }
    deriving stock Generic

instance ToJSON SearchResults where
    toJSON = genericToJSON $ aesonPrefix camelCase
    toEncoding = genericToEncoding $ aesonPrefix camelCase

-- | Search for @'Repo'@s by Owner or Name
--
-- This is just @owner|name ILIKE %{query}%@
--
searchRepos :: Int -> Text -> Handler SearchResults
searchRepos limit q = do
    page <- runDB $ paginateBy persistIdField Ascending limit $ searchFilters q
    settings <- view settingsL
    pure SearchResults
        { srPage = (\repo -> apiRepo repo settings Nothing) <$> page
        }

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
