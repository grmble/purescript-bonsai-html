-- | Bonsai HTML Internal module.
-- |
-- | Provides a smolder-style Html api, where child elements
-- | are nested in do-blocks and attributes are given
-- | with `!`.  Main difference to Smolder is the `msg`
-- | type is tracked that will be emitted by event handlers.
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
  , lazy
  , lazy2
  , lazy3
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
-- |
-- | Styles can be put on an element with the operators
-- | `#!` and `#!?`.  All styles will be collected
-- | and a style attribute will be put on the element.
type Style =
  { name :: String
  , value :: String
  }

-- | Create a custom attribute
-- |
-- | Attributes and properties are a long story, see
-- | https://github.com/elm-lang/html/blob/master/properties-vs-attributes.md
-- | for an explanation.
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
-- |
-- | Elements track their name, attributes and
-- | content aka child elements.
type Element msg =
  { name :: String
  , attribs :: CatList (VD.Property msg)
  , styles :: CatList Style
  , content :: Markup msg
  }

-- | KeyedElement record type
-- |
-- | KeyedElements have a key for every child element.
-- | This allows for virtual dom diffing/patching,
-- | and HTML elements for different keys will not
-- | be reused by the virtual dom
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
-- |
-- | `vnode` produces markup for a virtual dom
-- | node.  This allows interoperability between parts
-- | of a program where the Markup api is used,
-- | and parts where the bare virtual dom is used.
vnode :: forall msg. VD.VNode msg -> Markup msg
vnode v =
  liftF $ VNodeF v unit


-- | Create a text node.
-- |
-- |    p ! cls "warning" $ text "You have been warned."
text :: forall msg. String -> Markup msg
text s =
  vnode (VD.text s)


emptyMarkup :: forall msg. Markup msg
emptyMarkup = liftF $ EmptyF unit


-- | Create a leaf element - will not have children
-- |
-- | Note that for the HTML api this also means there can not be
-- | any text in it.
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
-- | This can be important when dealing with browser quirks,
-- | e.g. IE does not like it when the type of input elements
-- | changes.  Normally the Elm virtual dom is very aggressive
-- | about reusing existing elements, but for children of
-- | keyedNodes with different keys this will not happen.
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
      go (KeyedElementF rec x) = KeyedElementF (rec { attribs = snoc rec.attribs prop }) x
      go x = x
  withAttributes elem props =
    hoistFree go elem
    where
      go :: MarkupF msg ~> MarkupF msg
      go (ElementF rec x) = ElementF (rec { attribs = rec.attribs <> props }) x
      go (KeyedElementF rec x) = KeyedElementF (rec {attribs = rec.attribs <> props }) x
      go x = x

instance hasStyleMarkup :: HasStyle (Free (MarkupF msg) Unit) where
  withStyle elem st =
    hoistFree go elem
    where
      go :: MarkupF msg ~> MarkupF msg
      go (ElementF rec x) = ElementF (rec { styles = snoc rec.styles st }) x
      go (KeyedElementF rec x) = KeyedElementF (rec { styles = snoc rec.styles st }) x
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


-- | lazy markup.
-- |
-- | The elm virtual dom can have lazy nodes, they are only recomputed
-- | if a model changes (as this is javascript side, this usually means
-- | the object identity changes)
-- |
-- | These do not take `a -> Markup msg` functions because
-- | the virtual dom also checks that the function did not change.
-- | So, like for element handlers, you have to take care to pass
-- | something in that preserves object identity, like a top level function.
-- |
-- | Use for things like menus that only need a subset of your model
-- | as input.
-- |
-- |
-- |     lazy viewMenu model.active
-- |
lazy :: forall msg a. (a -> VD.VNode msg) -> a -> Markup msg
lazy f a =
  vnode $ VD.lazy f a

-- | Lazy markup with 2 arguments.
lazy2 :: forall msg a b. (a -> b -> VD.VNode msg) -> a -> b -> Markup msg
lazy2 f a b =
  vnode $ VD.lazy2 f a b

-- | Lazy markup with 3 arguments.
lazy3 :: forall msg a b c. (a -> b -> c -> VD.VNode msg) -> a  -> b -> c -> Markup msg
lazy3 f a b c =
  vnode $ VD.lazy3 f a b c
