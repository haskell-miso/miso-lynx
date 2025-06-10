-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.ScrollView.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.ScrollView.Property
  ( -- *** Property
    scrollOrientation_
  , enableScroll_
  , initialScrollOffset_
  , initialScrollToIndex_
  , bounces_
  , upperThreshold_
  , lowerThreshold_
  , scrollBarEnable_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | 'scrollOrientation_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#scroll-orientation>
--
-- // DefaultValue: "vertical"
-- scroll-orientation?: string
--
-- Set scroll orientation for the scrollable container.
--
scrollOrientation_ :: MisoString -> Attribute action
scrollOrientation_ = textProp "scroll-orientation"
-----------------------------------------------------------------------------
-- | 'enableScroll_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#enable-scroll>
--
-- // DefaultValue: true
-- enable-scroll?: boolean
--
-- Set scroll orientation for the scrollable container.
--
enableScroll_ :: Bool -> Attribute action
enableScroll_ = boolProp "enable-scroll"
-----------------------------------------------------------------------------
-- | 'initialScrollOffset_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#initial-scroll-offset>
--
-- // DefaultValue: N/A
-- initial-scroll-offset?: string = ${number}px
--
-- Set scroll orientation for the scrollable container.
--
initialScrollOffset_ :: MisoString -> Attribute action
initialScrollOffset_ = textProp "initial-scroll-offset"
-----------------------------------------------------------------------------
-- | 'initialScrollToIndex_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#initial-scroll-to-index>
--
-- // DefaultValue: N/A
-- initial-scroll-to-index?: string = ${number}px
--
-- Set scroll orientation for the scrollable container.
--
initialScrollToIndex_ :: MisoString -> Attribute action
initialScrollToIndex_ = textProp "initial-scroll-to-index"
-----------------------------------------------------------------------------
-- | 'bounces_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#bounces>
--
-- iOS only.
--
-- // DefaultValue: true
-- bounces?: boolean
--
-- Enable edge bounce effect.
--
bounces_ :: Bool -> Attribute action
bounces_ = boolProp "bounces"
-----------------------------------------------------------------------------
-- | 'upperThreshold_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#upper-threshold>
--
-- // DefaultValue: N/A
-- upper-threshold?: string = ${number}px
--
-- Sets a scroll threshold (unit: px), indicating how far from the top
-- or left before triggering the scrolltoupper event.
--
upperThreshold_ :: MisoString -> Attribute action
upperThreshold_ = textProp "upper-threshold"
-----------------------------------------------------------------------------
-- | 'lowerThreshold_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#lower-threshold>
--
-- // DefaultValue: N/A
-- lower-threshold?: string = ${number}px
--
-- Sets a scroll threshold (unit: px), indicating how far from the top
-- or left before triggering the scrolltolower event.
--
lowerThreshold_ :: MisoString -> Attribute action
lowerThreshold_ = textProp "lower-threshold"
-----------------------------------------------------------------------------
-- | 'scrollBarEnable_'
--
-- <https://lynxjs.org/api/elements/built-in/scroll-view.html#scroll-bar-enable>
--
-- // DefaultValue: N/A
-- lower-threshold?: string = ${number}px
--
-- Sets a scroll threshold (unit: px), indicating how far from the top
-- or left before triggering the scrolltolower event.
--
scrollBarEnable_ :: Bool -> Attribute action
scrollBarEnable_ = boolProp "scroll-bar-enable"
-----------------------------------------------------------------------------
