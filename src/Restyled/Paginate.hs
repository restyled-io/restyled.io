{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module Restyled.Paginate
  ( Direction (..)
  , paginateBy
  , Page (..)
  , renderPaginated
  , renderPagination
  ) where

import Restyled.Prelude hiding (sort)

import Lens.Micro ((^.))
import Restyled.Foundation
import Restyled.Paginate.Direction
import Restyled.Yesod
import Yesod.Page
import Yesod.Page.RenderedRoute

-- | Paginate on an arbitrary field in an arbitrary 'Direction'
--
-- This field must be indexed for the resulting queries to be performant.
--
-- Exmaple:
--
-- @
-- 'paginateBy' 'persistIdField' 'Ascending'
-- @
paginateBy
  :: ( MonadHandler m
     , PersistEntity entity
     , PersistEntityBackend entity ~ SqlBackend
     , Yesod (HandlerSite m)
     , PersistField a
     , ToJSON a
     , FromJSON a
     )
  => EntityField entity a
  -> Direction
  -> Int
  -> [Filter entity]
  -> SqlPersistT m (Page (Entity entity))
paginateBy field direction size userFilters =
  withPageAbsolute size (^. fieldLens field) $ \Cursor {..} -> do
    let
      filters = case directionWhere direction cursorPosition of
        Nothing -> userFilters
        Just f -> f field : userFilters

      options =
        [ LimitTo $ unLimit cursorLimit
        , directionOrderBy direction cursorPosition field
        ]

    directionSort direction cursorPosition <$> selectList filters options

renderPaginated :: (a -> Widget) -> Page a -> Widget
renderPaginated renderOne page =
  [whamlet|
    $if null $ pageData page
        <p>Hmm, nothing here yet...
    $else
        $forall item <- pageData page
            ^{renderOne item}

        ^{renderPagination page}
|]

renderPagination :: Page a -> Widget
renderPagination page =
  [whamlet|
<aside>
    <ul .pagination>
        $maybe p <- pagePrevious page
            <li .prev>
                <a href=#{renderedRouteText p}>«
        $nothing
            <li .prev .disabled>
                <a>«
        $maybe n <- pageNext page
            <li .next>
                <a href=#{renderedRouteText n}>»
        $nothing
            <li .next .disabled>
                <a>»
|]

renderedRouteText :: RenderedRoute -> Text
renderedRouteText r = case toJSON r of
  String t -> t
  _ -> error "RenderedRoute's ToJSON changed"
