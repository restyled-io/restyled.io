{-# OPTIONS_GHC -fno-warn-orphans #-}

module Restyled.Test.Graphula
    ( graph
    , onlyKey
    , generate
    , arbitrary
    , module Graphula
    ) where

import Restyled.Prelude

import Graphula
import Graphula.Arbitrary
import Restyled.Test.Yesod
import Test.QuickCheck.Arbitrary

instance MonadFail m => MonadFail (GraphulaT n m) where
    fail = lift . fail

graph
    :: HasSqlPool env
    => GraphulaT (YesodExample env) (YesodExample env) a
    -> YesodExample env ()
graph = void . runGraphulaT Nothing runDB

onlyKey :: Entity e -> Only (Key e)
onlyKey = only . entityKey
