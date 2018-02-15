-- | Bonsai HTML Internal module.
-- |
-- | Implements the HTML DSL.
-- | Heavily inspired by Smolder.
module Bonsai.Html.Internal
  ( class HasAttribute
  , class HasStyle
  , Style
  , KeyedContent
  , KeyedContentF
  , Markup
  , MarkupF
  , (!?)
  , (!)
  , (#!?)
  , (#!)
  , attribute
  , booleanProperty
  , keyed
  , keyedElement
  , leaf
  , mapMarkup
  , parent
  , render
  , render'
  , stringProperty
  , text
  , vnode
  , withAttribute
  , withAttributes
  , withOptionalAttribute
  , withOptionalStyle
  , withStyle
  )
where


import Prelude

import Bonsai.VirtualDom as VD
import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import Control.Monad.State (State, execState, state)
import Data.Array (fromFoldable)
import Data.CatList (CatList, empty, null, snoc)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))



-- Styles and Properties



-- | A style that will be collected for VD.style
type Style =
  { name :: String
  , value :: String
  }

-- | Create a custom attribute
attribute :: forall msg. String -> String -> VD.Property msg
attribute =
  VD.attribute

-- | Create a string property.
stringProperty :: forall msg. String -> String -> VD.Property msg
stringProperty =
  VD.property

-- | Create a boolean property.
booleanProperty :: forall msg. String -> Boolean -> VD.Property msg
booleanProperty =
  VD.property



-- Markup DSL ... smolder name, best name



-- | Element record type alias
type Element msg =
  { name :: String
  , attribs :: CatList (VD.Property msg)
  , styles :: CatList Style
  , content :: Markup msg
  }

type KeyedElement msg =
  { name :: String
  , attribs :: CatList (VD.Property msg)
  , styles :: CatList Style
  , nodes :: CatList (Tuple String (VD.VNode msg))
  }


-- | Markup Functor for the DSL
data MarkupF msg a
  = ElementF (Element msg) a
  | KeyedElementF (KeyedElement msg) a
  | VNodeF (VD.VNode msg) a
  | EmptyF a

instance functorMarkupF :: Functor (MarkupF msg) where
  map f (ElementF rec x) = ElementF rec (f x)
  map f (KeyedElementF rec x) = KeyedElementF rec (f x)
  map f (VNodeF v x) = VNodeF v (f x)
  map f (EmptyF x) = EmptyF (f x)


type Markup msg = Free (MarkupF msg) Unit


-- | KeyedContent Functor for the DSL
data KeyedContentF msg a
  = KeyedContentF (Tuple String (VD.VNode msg)) a
  | KeyedEmptyF a


instance functorKeyedContentF :: Functor (KeyedContentF msg) where
  map f (KeyedContentF tup x) = KeyedContentF tup (f x)
  map f (KeyedEmptyF x) = KeyedEmptyF (f x)


type KeyedContent msg = Free (KeyedContentF msg) Unit



-- | Wrap a (already rendered?) VNode
vnode :: forall msg. VD.VNode msg -> Markup msg
vnode v =
  liftF $ VNodeF v unit


-- | Create a text node.
text :: forall msg. String -> Markup msg
text s =
  vnode (VD.text s)


emptyMarkup :: forall msg. Markup msg
emptyMarkup = liftF $ EmptyF unit


-- | Create a leaf element - will not have children
leaf :: forall msg. String -> Markup msg
leaf name =
  liftF $ ElementF { name, attribs: empty, styles: empty, content: emptyMarkup } unit


-- | Create an element with children.
parent
  :: forall msg
  .  String
  -> Markup msg
  -> Markup msg
parent name content =
  liftF $ ElementF { name, attribs: empty, styles: empty, content: content } unit

-- | keyedElement renders to a VirtualDom keyedNode.
-- |
-- | This means the virtual dom can be more intelligent
-- | when diffing.  Also keyed elements will not be reused
-- | by nodes with different ids (e.g. when one node is
-- | deleted).
-- |
-- |
-- |      keyedElement "ul" ! cls "klass" $ do
-- |        keyed ("K1") (VD.node "li" [ id_ "K1"] [ VD.text "K1" ])
-- |        keyed ("K2") (VD.node "li" [ id_ "K2"] [ VD.text "K2" ])
keyedElement
  :: forall msg
  .  String
  -> KeyedContent msg
  -> Markup msg
keyedElement name keyedChildren =
  let nodes = renderKeyedContent' keyedChildren
  in  liftF $ KeyedElementF { name, attribs: empty, styles: empty, nodes } unit


-- | A content element for keyed node.
keyed
  :: forall msg
  .  String
  -> VD.VNode msg
  -> KeyedContent msg
keyed k n =
  liftF $ KeyedContentF (Tuple k n) unit

-- DSL for attributes/styles

class HasAttribute a b | a -> b where
  -- | Add an attribute to element node
  withAttribute :: a -> b -> a

  -- | Append a list of attributes to the element node
  -- |
  -- | Performce helper for code that produces Markup
  withAttributes :: a -> CatList b -> a

class HasStyle a where
  -- | Add a style to an element node
  withStyle :: a -> Style -> a

instance hasAttributeMarkup :: HasAttribute (Free (MarkupF msg) Unit) (VD.Property msg) where
  withAttribute elem prop =
    hoistFree go elem
    where
      go :: MarkupF msg ~> MarkupF msg
      go (ElementF rec x) = ElementF (rec { attribs = snoc rec.attribs prop }) x
      go x = x
  withAttributes elem props =
    hoistFree go elem
    where
      go :: MarkupF msg ~> MarkupF msg
      go (ElementF rec x) = ElementF (rec { attribs = rec.attribs <> props }) x
      go x = x

instance hasStyleMarkup :: HasStyle (Free (MarkupF msg) Unit) where
  withStyle elem st =
    hoistFree go elem
    where
      go :: MarkupF msg ~> MarkupF msg
      go (ElementF rec x) = ElementF (rec { styles = snoc rec.styles st }) x
      go x = x

-- the other 2 instances are because `div ! cls "parent" $ div $ text "child"`
-- will be a function Markup -> Markup for our operator

instance hasAttributeMarkupF
  :: HasAttribute (Free (MarkupF msg) Unit -> Free (MarkupF msg) Unit) (VD.Property msg) where
  withAttribute efn prop elem =
    withAttribute (efn elem) prop
  withAttributes efn props elem =
    withAttributes (efn elem) props

instance hasStyleMarkupF
  :: HasStyle (Free (MarkupF msg) Unit -> Free (MarkupF msg) Unit) where
  withStyle efn prop elem =
    withStyle (efn elem) prop


instance hasAttributeKeyedContentF
  :: HasAttribute (Free (KeyedContentF msg) Unit -> Free (MarkupF msg) Unit) (VD.Property msg) where
  withAttribute efn prop elem =
    withAttribute (efn elem) prop
  withAttributes efn props elem =
    withAttributes (efn elem) props


instance hasStyleKeyedContentF
  :: HasStyle (Free (KeyedContentF msg) Unit -> Free (MarkupF msg) Unit) where
  withStyle efn prop elem =
    withStyle (efn elem) prop



-- | Add an optional attribute (operator !?)
withOptionalAttribute
  :: forall h msg
  .  HasAttribute h (VD.Property msg)
  => h
  -> Maybe (VD.Property msg)
  -> h
withOptionalAttribute elem Nothing =
  elem
withOptionalAttribute elem (Just prop) =
  withAttribute elem prop

-- ! Add an optional style (operator #!?)
withOptionalStyle
  :: forall h
  .  HasStyle h
  => h
  -> Maybe Style
  -> h
withOptionalStyle elem Nothing =
  elem
withOptionalStyle elem (Just st) =
  withStyle elem st

infixl 4 withAttribute as !
infixl 4 withOptionalAttribute as !?
infixl 4 withStyle as #!
infixl 4 withOptionalStyle as #!?


-- RENDERING

-- | Render the content DSL to a VirtualDom node.
render :: forall msg. Markup msg -> VD.VNode msg
render elem =

  singleNode $ render' elem

  where

    singleNode :: Array (VD.VNode msg) -> VD.VNode msg
    singleNode ns =
      case ns of
        [] ->
          VD.node "div" [] []
        [ n ] ->
          n
        x ->
          VD.node "div" [] x



-- | Render the content DSL to an Array of VirtualDom nodes.
render' :: forall msg. Markup msg -> Array (VD.VNode msg)
render' elem =

  renderNodes elem

  where

    styles2Tups s = map (\st -> Tuple st.name st.value) s

    attribsAndStyles attribs styles =
      if null styles
        then attribs
        else snoc attribs (VD.style $ fromFoldable $ styles2Tups styles)

    renderNodes :: Markup msg -> Array (VD.VNode msg)
    renderNodes content =
      fromFoldable $ execState (foldFree foldMarkupF content) empty

    foldMarkupF :: MarkupF msg ~> State (CatList (VD.VNode msg))
    foldMarkupF (EmptyF x) =
      pure x
    foldMarkupF (VNodeF vn x) =
      state \acc -> Tuple x (snoc acc vn)
    foldMarkupF (KeyedElementF rec x) =
      state \acc ->
        Tuple x $ snoc acc $
          VD.keyedNode
            rec.name
            (fromFoldable $ attribsAndStyles rec.attribs rec.styles)
            (fromFoldable rec.nodes)
    foldMarkupF (ElementF rec x) =
      let
        c = renderNodes rec.content
        a = attribsAndStyles rec.attribs rec.styles
      in
        state \acc -> Tuple x (snoc acc (VD.node rec.name (fromFoldable a) c))


renderKeyedContent' :: forall msg. KeyedContent msg -> CatList (Tuple String (VD.VNode msg))
renderKeyedContent' content =
  execState (foldFree foldContentF content) empty

  where

    foldContentF :: KeyedContentF msg ~> State (CatList (Tuple String (VD.VNode msg)))
    foldContentF (KeyedEmptyF x) =
      pure x
    foldContentF (KeyedContentF tup x) =
      state \acc -> Tuple x (snoc acc tup)


-- | Map the markup from one message type to another.
mapMarkup :: forall msg1 msg2. (msg1 -> msg2) -> Markup msg1 -> Markup msg2
mapMarkup fn markup =
  vnode $ map fn (render markup)
