module Test.Markup where

import Prelude

import Bonsai (BONSAI, Cmd(..), ElementId(..), program)
import Bonsai.Core (issueCommand')
import Bonsai.DOM (DOM, affF, document, elementById, innerHTML, textContent)
import Bonsai.Html (KeyedContentF, button, div_, input, keyed, keyedElement, p, render, text, (!))
import Bonsai.Html.Attributes (cls, id_, typ, value)
import Bonsai.Html.Events (onClick, onInput)
import Bonsai.JSDOM (jsdomWindow, setValue, simulantFire)
import Bonsai.VirtualDom as VD
import Control.Monad.Aff (liftEff')
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Free (Free)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Plus (empty)
import Data.Tuple (Tuple(..))
import Test.Unit (TestF, suite, test)
import Test.Unit.Assert as Assert


--
--
--
-- example for render/input/click test
--
--
--


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


view :: Model -> VD.VNode Msg
view model =
  render do
    div_ do
      input ! id_ "I1" ! typ "text" ! value model.text ! onInput Input
      button ! id_ "B1" ! typ "button" ! onClick Click $ text "Click"
    p $ text "Result:"
    p ! id_ "RT" $ text model.text
    p ! id_ "RC" $ text $ show model.clickCount





--
--
-- example for keyedElement test - just a static rendering of a keyedElement
--
--

data KeyedMsg
  = Inc
  | Dec


keyedUpdate :: forall eff. KeyedMsg -> Int -> Tuple (Cmd eff KeyedMsg) Int
keyedUpdate Inc i =
  Tuple empty (i + 1)
keyedUpdate Dec i =
  Tuple empty (i - 1)


keyedView :: Int -> VD.VNode KeyedMsg
keyedView count =
  render $
    keyedElement "ul" ! cls "klass" $
      tailRecM go 0

  where
    go :: Int -> Free (KeyedContentF KeyedMsg) (Step Int Unit)
    go i =
      if i < count
        then do
          keyed ("K" <> show i) (VD.node "li" [ id_ ("K" <> show i)] [ VD.text $ show i ])
          pure $ Loop (i + 1)
        else
          pure $ Done unit



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

    test "keyedElement" $ do
      let win = jsdomWindow """<html><body id="main"></body></html>"""
      doc <- affF $ win >>= document
      prg <- liftEff' $ program (ElementId "main") keyedUpdate keyedView 3 win

      k2 <- affF $ elementById (ElementId "K2") doc >>= innerHTML
      Assert.equal "2" k2
