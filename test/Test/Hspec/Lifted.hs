module Test.Hspec.Lifted
    ( module X
    , module Test.Hspec.Lifted
    ) where

import Prelude

import Control.Monad.IO.Class
import Test.Hspec as Hspec
import Test.Hspec as X hiding
    ( expectationFailure
    , pending
    , pendingWith
    , shouldBe
    , shouldContain
    , shouldEndWith
    , shouldMatchList
    , shouldNotBe
    , shouldNotContain
    , shouldNotReturn
    , shouldNotSatisfy
    , shouldReturn
    , shouldSatisfy
    , shouldStartWith
    )
import Test.Hspec.Expectations.Lifted as X
    ( expectationFailure
    , shouldBe
    , shouldContain
    , shouldEndWith
    , shouldMatchList
    , shouldNotBe
    , shouldNotContain
    , shouldNotReturn
    , shouldNotSatisfy
    , shouldReturn
    , shouldSatisfy
    , shouldStartWith
    )

pending :: MonadIO m => m ()
pending = liftIO Hspec.pending

pendingWith :: MonadIO m => String -> m ()
pendingWith = liftIO . Hspec.pendingWith
