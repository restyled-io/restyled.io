module Test.Hspec.Lifted
    ( module X
    )
where

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
