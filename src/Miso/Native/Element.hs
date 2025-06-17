-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element
  ( -- ** Smart constructor for native elements
    native_
    -- ** Page
  , page_
    -- ** View
  , view_
    -- ** Scroll View
  , scrollView_
    -- ** Image
  , image_
    -- ** List
  , list_
  , listItem_
    -- * Text
  , text_
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson (toJSON)
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (View, Attribute, node, NS(HTML))
import           Miso.Property (textProp, prop)
import           Miso.Native.Element.List (ListOptions(..))
-----------------------------------------------------------------------------
-- | Smart constructor for constructing a built-in native element.
--
native_ :: MisoString -> [Attribute action] -> [View action] -> View action
native_ = node HTML
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/page.html>
--
-- <page> element is the root node, only one <page> element is allowed per page.
-- You can omit the explicit <page> wrapper, as the frontend framework will
-- generate the root node by default.
--
-- You shouldn't use this, we already generate the 'page' for you when
-- the initial 'renderPage' callback is invoked by PrimJS, and there can
-- only be one 'page' present at at time. We include it here for completeness,
-- and because 'page' functionality might change in the future.
--
page_ :: [Attribute action] -> [View action] -> View action
page_ = native_ "page"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/scroll-view.html>
--
-- Basic element, used to contain other elements. <view> is the foundation
-- for all other elements; its attributes, events, and methods can be
-- used in other elements.
--
scrollView_ :: [Attribute action] -> [View action] -> View action
scrollView_ = native_ "scroll-view"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/view.html>
--
-- Basic element, used to contain other elements. <view> is the foundation
-- for all other elements; its attributes, events, and methods can be
-- used in other elements.
--
view_ :: [Attribute action] -> [View action] -> View action
view_ = native_ "view"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html>
--
-- Used to display different types of images, including web images,
-- static resources, and locally stored images.
--
-- <https://lynxjs.org/api/elements/built-in/image.html>
--
-- 'image_' does not support children.
--
-- <https://lynxjs.org/api/elements/built-in/image.html#required-src>
--
-- *Required*
--
-- 'image_' takes a required *src* parameter (as 'MisoString') by default.
--
-- The supported image formats are: *png*, *jpg*, *jpeg*, *bmp*, *gif*, and *webp*.
--
-- > image_ "https://url.com/image.png" []
--
image_ :: MisoString -> [Attribute action] -> View action
image_ url attrs = native_ "image" (textProp "src" url : attrs) []
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/list.html>
--
listItem_ :: [Attribute action] -> [View action] -> View action
listItem_ = native_ "list-item"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/list.html>
--
list_ :: ListOptions -> [Attribute action] -> [View action] -> View action
list_ ListOptions {..} attrs = native_ "list" (defaults <> attrs)
  where
    defaults =
      [ prop "list-type" (toJSON listType_)
      , prop "span-count" (toJSON spanCount_)
      , prop "scroll-orientation" (toJSON scrollOrientation_)
      ]
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/text.html>
--
-- <text> is a built-in component in Lynx used to display text content.
-- It supports specifying text style, binding click event callbacks, and can
-- nest <text>, <image>, and <view> components to achieve relatively complex
-- text and image content presentation.
--
text_ :: [Attribute action] -> [View action] -> View action
text_ = native_ "text"
-----------------------------------------------------------------------------
