module Restyled.Paginate.Direction
    ( Direction(..)
    , directionWhere
    , directionOrderBy
    , directionSort
    ) where

import Restyled.Prelude

import Yesod.Page as Page

data Direction = Ascending | Descending

directionWhere
    :: PersistField a
    => Direction
    -> Position a
    -> Maybe (EntityField entity a -> Filter entity)
directionWhere = \case
    Ascending -> \case
        Page.First -> Nothing
        Page.Previous p -> Just (<. p)
        Page.Next p -> Just (>. p)
        Page.Last -> Nothing
    Descending ->
        (\case
            Page.First -> Nothing
            Page.Previous p -> Just (>. p)
            Page.Next p -> Just (<. p)
            Page.Last -> Nothing
        )

directionOrderBy
    :: Direction
    -> Position position
    -> EntityField entity a
    -> SelectOpt entity
directionOrderBy = \case
    Ascending -> \case
        Page.First -> Asc
        Page.Previous _ -> Desc
        Page.Next _ -> Asc
        Page.Last -> Desc
    Descending -> \case
        Page.First -> Desc
        Page.Previous _ -> Asc
        Page.Next _ -> Desc
        Page.Last -> Asc

directionSort :: Direction -> Position position -> [a] -> [a]
directionSort = \case
    Ascending -> \case
        Page.First -> id
        Page.Previous _ -> reverse
        Page.Next _ -> id
        Page.Last -> reverse
    Descending -> \case
        Page.First -> reverse
        Page.Previous _ -> id
        Page.Next _ -> reverse
        Page.Last -> id
