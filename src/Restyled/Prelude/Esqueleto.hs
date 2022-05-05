module Restyled.Prelude.Esqueleto
    ( module X
    , selectMap
    , selectFoldMap
    , aggregate
    , unValue2
    , unValue3
    , unValue5
    ) where

import Restyled.Prelude as X hiding
    ( LogFunc
    , SqlEntity
    , Value
    , count
    , delete
    , exists
    , isNothing
    , on
    , selectSource
    , set
    , update
    , (!=.)
    , (*=.)
    , (+=.)
    , (-=.)
    , (/=.)
    , (<&>)
    , (<.)
    , (<=.)
    , (=.)
    , (==.)
    , (>.)
    , (>=.)
    , (^.)
    , (||.)
    )

import Database.Esqueleto.Legacy as X

import Database.Esqueleto.Internal.Internal (SqlSelect)
import Database.Esqueleto.PostgreSQL

selectMap
    :: (MonadIO m, SqlSelect a r) => (r -> b) -> SqlQuery a -> SqlPersistT m [b]
selectMap f q = map f <$> select q

selectFoldMap
    :: (MonadIO m, SqlSelect a r, Monoid b)
    => (r -> b)
    -> SqlQuery a
    -> SqlPersistT m b
selectFoldMap f q = foldMap f <$> select q

aggregate
    :: (PersistField a, PersistField [a])
    => SqlExpr (Value a)
    -> SqlExpr (Value [a])
aggregate = maybeArray . arrayAgg

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (Value a, Value b) = (a, b)

unValue3 :: (Value a, Value b, Value c) -> (a, b, c)
unValue3 (Value a, Value b, Value c) = (a, b, c)

unValue5 :: (Value a, Value b, Value c, Value d, Value e) -> (a, b, c, d, e)
unValue5 (Value a, Value b, Value c, Value d, Value e) = (a, b, c, d, e)
