module Test.Markup where

import Prelude

import Bonsai (Cmd(..), ElementId(..), program)
import Bonsai.Core (issueCommand')
import Bonsai.DOM (affF, document, elementById, innerHTML, textContent)
import Bonsai.Html (KeyedContentF, button, div_, input, keyed, keyedElement, lazy, lazy2, lazy3, p, render, text, (!))
import Bonsai.Html.Attributes (cls, id_, typ, value)
import Bonsai.Html.Events (onClick, onInput)
import Bonsai.JSDOM (fireClick, fireInput, jsdomWindow, setValue)
import Bonsai.VirtualDom as VD
import Effect.Class (liftEffect)
import Control.Monad.Free (Free)
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Plus (empty)
import Foreign (readString)
import Foreign.Index as FI
import Data.Newtype (unwrap)
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


update :: Msg -> Model -> Tuple (Cmd Msg) Model
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


keyedUpdate :: KeyedMsg -> Int -> Tuple (Cmd KeyedMsg) Int
keyedUpdate Inc i =
  Tuple empty (i + 1)
keyedUpdate Dec i =
  Tuple empty (i - 1)


keyedView :: Int -> VD.VNode KeyedMsg
keyedView count =
  render $
    keyedElement "ul" ! id_ "UL" ! cls "klass" $
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


--
--
--  example for the lazy test
--
--
type LazyModel =
  { l1 :: LazyChoice
  , l2 :: LazyChoice
  , l3 :: LazyChoice
  }

data LazyChoice
  = LazyOne
  | LazyTwo

instance showLazyChoice :: Show LazyChoice where
  show (LazyOne) = "C1"
  show (LazyTwo) = "C2"


data LazyMsg
  = L1 LazyChoice
  | L2 LazyChoice
  | L3 LazyChoice


lazyUpdate :: LazyMsg -> LazyModel -> Tuple (Cmd LazyMsg) LazyModel
lazyUpdate msg model =
  Tuple empty
    case msg of
      L1 lc -> model { l1 = lc }
      L2 lc -> model { l2 = lc }
      L3 lc -> model { l3 = lc }

lazyView :: LazyModel -> VD.VNode LazyMsg
lazyView model =
  render $ div_ do
    lazy lazyViewL1 model.l1
    lazy2 lazyViewL1L2 model.l1 model.l2
    lazy3 lazyViewL1L2L3 model.l1 model.l2 model.l3


-- top level to make sure they are not changing between renders
lazyViewL1 :: LazyChoice -> VD.VNode LazyMsg
lazyViewL1 l1 =
  render $ p ! id_ "L1" $ text ("L1" <> show l1)
lazyViewL1L2 :: LazyChoice -> LazyChoice -> VD.VNode LazyMsg
lazyViewL1L2 l1 l2 =
  render $ p ! id_ "L1L2" $ text ("L1" <> show l1 <> ",L2" <> show l2)
lazyViewL1L2L3 :: LazyChoice -> LazyChoice -> LazyChoice -> VD.VNode LazyMsg
lazyViewL1L2L3 l1 l2 l3 =
  render $ p ! id_ "L1L2L3" $ text ("L1" <> show l1 <> ",L2" <> show l2 <> ",L3" <> show l3)





tests :: Free TestF Unit
tests =
  suite "Bonsai.Html" do
    test "render/input/click" $ do
      let win = jsdomWindow """<html><body id="main"></body></html>"""
      doc <- affF $ win >>= document
      prg <- liftEffect $ program (ElementId "main") update view emptyModel win

      initialText <- affF $ elementById (ElementId "RT") doc >>= textContent
      initialCount <- affF $ elementById (ElementId "RC") doc >>= textContent
      Assert.equal "" initialText
      Assert.equal "0" initialCount

      -- fire button click event
      affF $ elementById (ElementId "B1") doc >>= fireClick
      issueCommand' prg $ Cmd [] -- will wait for render
      count <- affF $ elementById (ElementId "RC") doc >>= textContent
      Assert.equal "1" count

      -- set text input value and fire input input
      affF $ elementById (ElementId "I1") doc >>= setValue "blubb"
      affF $ elementById (ElementId "I1") doc >>= fireInput
      issueCommand' prg $ Cmd [] -- will wait for render
      blubb <- affF $ elementById (ElementId "RT") doc >>= textContent
      Assert.equal "blubb" blubb

    test "keyedElement" $ do
      let win = jsdomWindow """<html><body id="main"></body></html>"""
      doc <- affF $ win >>= document
      prg <- liftEffect $ program (ElementId "main") keyedUpdate keyedView 3 win

      {--
      main <- affF $ elementById (ElementId "main") doc >>= innerHTML
      log main
      --}

      ulKlass <- affF $ elementById (ElementId "UL") doc >>=
                      -- class attribute is className property.
                      (\e -> unwrap e FI.! "className") >>=
                      readString
      k2 <- affF $ elementById (ElementId "K2") doc >>= innerHTML

      Assert.equal "klass" ulKlass
      Assert.equal "2" k2



    test "lazy/lazy2/lazy3" $ do
      let win = jsdomWindow """<html><body id="main"></body></html>"""
      doc <- affF $ win >>= document
      prg <- liftEffect $ program (ElementId "main")
                lazyUpdate lazyView {l1: LazyOne, l2: LazyOne, l3: LazyOne}
                win

      l1 <- affF $ elementById (ElementId "L1") doc >>= textContent
      l12 <- affF $ elementById (ElementId "L1L2") doc >>= textContent
      l123 <- affF $ elementById (ElementId "L1L2L3") doc >>= textContent
      Assert.equal "L1C1" l1
      Assert.equal "L1C1,L2C1" l12
      Assert.equal "L1C1,L2C1,L3C1" l123

      issueCommand' prg $ pure (L1 LazyTwo)
      l1' <- affF $ elementById (ElementId "L1") doc >>= textContent
      l12' <- affF $ elementById (ElementId "L1L2") doc >>= textContent
      l123' <- affF $ elementById (ElementId "L1L2L3") doc >>= textContent
      Assert.equal "L1C2" l1'
      Assert.equal "L1C2,L2C1" l12'
      Assert.equal "L1C2,L2C1,L3C1" l123'
