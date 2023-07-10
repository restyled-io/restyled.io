module Restyled.Prelude.Esqueleto
  ( module X
  , selectMap
  , selectFoldMap
  , aggregate
  , stringEqual
  , unValue2
  , unValue3
  , unValue5
  ) where

import Restyled.Prelude as X hiding
  ( SqlEntity
  , Value
  , count
  , delete
  , exists
  , isNothing
  , on
  , selectSource
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
  , (||.)
  )

import Database.Esqueleto.Legacy as X

import Database.Esqueleto.Internal.Internal (SqlSelect, unsafeSqlBinOp)
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

-- | Compare two values by their string representation for equality
stringEqual
  :: forall s t
   . (SqlString s, SqlString t)
  => SqlExpr (Value s)
  -> SqlExpr (Value t)
  -> SqlExpr (Value Bool)
stringEqual x y =
  unsafeSqlBinOp
    " = "
    (constrainMe @(SqlString s) x)
    (constrainMe @(SqlString t) y)

-- Matches (==.)
infix 4 `stringEqual`

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (Value a, Value b) = (a, b)

unValue3 :: (Value a, Value b, Value c) -> (a, b, c)
unValue3 (Value a, Value b, Value c) = (a, b, c)

unValue5 :: (Value a, Value b, Value c, Value d, Value e) -> (a, b, c, d, e)
unValue5 (Value a, Value b, Value c, Value d, Value e) = (a, b, c, d, e)
