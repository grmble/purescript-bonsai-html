module Test.Markup where

import Prelude

import Bonsai (BONSAI, Cmd(..), ElementId(..), program)
import Bonsai.Core (issueCommand')
import Bonsai.DOM (DOM, affF, document, elementById, textContent)
import Bonsai.Html (button, div_, input, p, render, text, (!))
import Bonsai.Html.Attributes (id_, typ, value)
import Bonsai.Html.Events (onClick, onInput)
import Bonsai.JSDOM (jsdomWindow, setValue, simulantFire)
import Bonsai.VirtualDom (VNode)
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free)
import Control.Plus (empty)
import Data.Tuple (Tuple(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert

data Msg
  = Input String
  | Click


type Model =
  { text :: String
  , clickCount :: Int
  }


emptyModel :: Model
emptyModel =
  { text: "", clickCount: 0 }


update :: forall eff. Msg -> Model -> Tuple (Cmd eff Msg) Model
update (Input s) model =
  Tuple empty $ model { text = s }
update (Click) model =
  Tuple empty $ model { clickCount = model.clickCount + 1 }


view :: Model -> VNode Msg
view model =
  render do
    div_ do
      input ! id_ "I1" ! typ "text" ! value model.text ! onInput Input
      button ! id_ "B1" ! typ "button" ! onClick Click $ text "Click"
    p $ text "Result:"
    p ! id_ "RT" $ text model.text
    p ! id_ "RC" $ text $ show model.clickCount




tests :: forall eff. Free (TestF (bonsai::BONSAI,dom::DOM,console::CONSOLE|eff)) Unit
tests =
  suite "Bonsai.Html" do
    test "render/input/click" $ do
      let win = jsdomWindow """<html><body id="main"></body></html>"""
      doc <- affF $ win >>= document
      prg <- liftEff' $ program (ElementId "main") update view emptyModel win

      initialText <- affF $ elementById (ElementId "RT") doc >>= textContent
      initialCount <- affF $ elementById (ElementId "RC") doc >>= textContent
      Assert.equal "" initialText
      Assert.equal "0" initialCount

      -- fire button click event
      affF $ elementById (ElementId "B1") doc >>= simulantFire "click"
      issueCommand' prg $ Cmd [] -- will wait for render
      count <- affF $ elementById (ElementId "RC") doc >>= textContent
      Assert.equal "1" count

      -- set text input value and fire input input
      affF $ elementById (ElementId "I1") doc >>= setValue "blubb"
      affF $ elementById (ElementId "I1") doc >>= simulantFire "input"
      issueCommand' prg $ Cmd [] -- will wait for render
      blubb <- affF $ elementById (ElementId "RT") doc >>= textContent
      Assert.equal "blubb" blubb
