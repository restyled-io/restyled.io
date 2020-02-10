module Restyled.Prelude.Esqueleto
    ( module X
    , selectMap
    , unValue2
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

import Database.Esqueleto.Internal.Sql (SqlSelect)

selectMap
    :: (MonadIO m, SqlSelect a r) => (r -> b) -> SqlQuery a -> SqlPersistT m [b]
selectMap f = fmap (map f) . select

unValue2 :: (Value a, Value b) -> (a, b)
unValue2 (Value a, Value b) = (a, b)
