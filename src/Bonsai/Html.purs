-- | Bonsai Html Markup
-- |
-- | View code is expected to import this module plus
-- | Bonsai.Html.Attributes and Bonsai.Html.Events
module Bonsai.Html
  ( module Bonsai.Html.Internal
  , module Bonsai.VirtualDom
  , module E
  )
where

import Bonsai.Html.Internal (KeyedContent, KeyedContentF, Markup, MarkupF, (!), (!?), (#!), (#!?), attribute, keyed, keyedElement, lazy, lazy2, lazy3, mapMarkup, text, render, render', vnode)
import Bonsai.VirtualDom (VNode, Property, on, onWithOptions, defaultOptions)
import Bonsai.Html.Elements as E
