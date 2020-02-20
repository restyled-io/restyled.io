module Restyled.Prelude.Esqueleto
    ( module X
    , selectMap
    , refineNotNull
    , unValue2
    , unValue5
    )
where

import Restyled.Prelude as X hiding
    ( LogFunc
    , SqlEntity
    , Value
    , count
    , delete
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

import Database.Esqueleto as X

import Database.Esqueleto.Internal.Sql (SqlSelect, veryUnsafeCoerceSqlExprValue)

selectMap
    :: (MonadIO m, SqlSelect a r) => (r -> b) -> SqlQuery a -> SqlPersistT m [b]
selectMap f = fmap (map f) . select

refineNotNull
    :: PersistField a
    => SqlExpr (Value (Maybe a))
    -> SqlQuery (SqlExpr (Value a))
refineNotNull m = do
    where_ $ not_ $ isNothing m
    pure $ veryUnsafeCoerceSqlExprValue m

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (Value a, Value b) = (a, b)

unValue5 :: (Value a, Value b, Value c, Value d, Value e) -> (a, b, c, d, e)
unValue5 (Value a, Value b, Value c, Value d, Value e) = (a, b, c, d, e)
