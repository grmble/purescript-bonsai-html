module Test.Main where

import Prelude

import Control.Monad.Free (Free)
import Effect (Effect)
import Test.Markup as TMarkup
import Test.Unit (TestF)
import Test.Unit.Main (runTest)


main :: Effect Unit
main = runTest tests



tests :: Free TestF Unit
tests = do
  TMarkup.tests
