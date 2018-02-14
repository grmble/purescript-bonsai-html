module Test.Main where

import Prelude

import Bonsai (BONSAI)
import Bonsai.DOM (DOM)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free)
import Test.Markup as TMarkup
import Test.Unit (TestF)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)


main :: forall t1.
  Eff
    ( avar :: AVAR
    , bonsai :: BONSAI
    , console :: CONSOLE
    , dom :: DOM
    , testOutput :: TESTOUTPUT
    | t1
    )
    Unit
main = runTest tests



tests :: forall eff. Free (TestF (bonsai::BONSAI,console::CONSOLE,dom::DOM|eff)) Unit
tests = do
  TMarkup.tests
