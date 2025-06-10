-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.List.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.List.Property
  ( -- *** Property
    -- *** Types
    ListOptions (..)
  , ScrollOrientation (..)
  , ListType (..)
    -- *** Defaults
  , defaultListOptions
    -- *** Attributes
  , enableScroll_
  , enableNestedScroll_
  , listMainAxisGap_
  , listCrossAxisGap_
  , sticky_
  , stickyOffset_
  , stickyTop_
  , stickyBottom_
  , initialScrollIndex_
  , needVisibleItemInfo_
  , upperThresholdItemCount_
  , lowerThresholdItemCount_
  , scrollEventThrottle_
  , layoutId_
  , itemKey_
  , key_
  ) where
-----------------------------------------------------------------------------
import Data.Aeson
-----------------------------------------------------------------------------
import Miso.String (MisoString)
import Miso.Types (Attribute)
import Miso.Property
-----------------------------------------------------------------------------
-- | ListOptions
data ListOptions
  = ListOptions
  { listType_ :: ListType
    -- ^ list-type: 'single' | 'flow' | 'waterfall'
  , spanCount_ :: Int
    -- ^ Sets the number of columns or rows for the <list> component layout.
  , scrollOrientation_ :: ScrollOrientation
    -- ^ scroll-orientation?: 'vertical' ｜ 'horizontal'
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | ScrollOrientation
data ScrollOrientation
  = Vertical
  | Horizontal
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON ScrollOrientation where
  toJSON Vertical   = "vertical"
  toJSON Horizontal = "horizontal"
-----------------------------------------------------------------------------
-- | ListType
data ListType
  = Single
  | Flow
  | Waterfall
  deriving (Show, Eq)
-----------------------------------------------------------------------------
instance ToJSON ListType where
  toJSON Single    = "single"
  toJSON Flow      = "flow"
  toJSON Waterfall = "waterfall"
-----------------------------------------------------------------------------
-- | defaultListOptions
defaultListOptions :: ListOptions
defaultListOptions
  = ListOptions
  { listType_ = Single
  , spanCount_ = 0
  , scrollOrientation_ = Vertical
  }
-----------------------------------------------------------------------------
-- | 'itemKey_'
--
-- <>
--
-- // DefaultValue: true
-- enable-scroll?: boolean
--
-- Indicates whether the `<list>` component is allowed to scroll.
--
itemKey_ :: MisoString -> Attribute action
itemKey_ = textProp "item-key"
-----------------------------------------------------------------------------
-- | 'enableScroll_'
--
-- <>
--
-- // DefaultValue: true
-- enable-scroll?: boolean
--
-- Indicates whether the `<list>` component is allowed to scroll.
--
enableScroll_ :: Bool -> Attribute action
enableScroll_ = boolProp "enable-scroll"
-----------------------------------------------------------------------------
-- | 'enableNestedScroll_'
--
-- <>
--
-- // DefaultValue: true
-- enable-nested-scroll?: boolean
--
-- Indicates whether `<list>` can achieve nested scrolling with other scrollable containers. When enabled, the inner container scrolls first, followed by the outer container.
--
enableNestedScroll_ :: Bool -> Attribute action
enableNestedScroll_ = boolProp "enable-nested-scroll"
-----------------------------------------------------------------------------
-- | 'listMainAxisGap_'
--
-- <>
--
-- // DefaultValue: null
-- list-main-axis-gap?: ${number}px | ${number}rpx
--
-- Specifies the spacing of `<list>` child nodes in the main axis direction,
-- which needs to be written in the style.
--
listMainAxisGap_ :: MisoString -> Attribute action
listMainAxisGap_ = textProp "list-main-axis-gap"
-----------------------------------------------------------------------------
-- | 'listCrossAxisGap_'
--
-- <>
--
-- // DefaultValue: null
-- list-main-axis-gap?: ${number}px | ${number}rpx
--
-- Specifies the spacing of `<list>` child nodes in the main axis direction,
-- which needs to be written in the style.
--
listCrossAxisGap_ :: MisoString -> Attribute action
listCrossAxisGap_ = textProp "list-cross-axis-gap"
-----------------------------------------------------------------------------
-- | 'sticky_'
--
-- <>
--
-- // DefaultValue: false
-- sticky?: boolean
--
-- Declared on the `<list>` component to control whether the `<list>` component
-- as a whole is allowed to be sticky at the top or bottom.
--
sticky_ :: Bool -> Attribute action
sticky_ = boolProp "sticky"
-----------------------------------------------------------------------------
-- | 'stickyOffset_'
--
-- <>
--
-- // DefaultValue: 0
-- stickyOffset?: number
--
-- The offset distance from the top or bottom of `<list>` for sticky positioning, in `px`.
--
stickyOffset_ :: Int -> Attribute action
stickyOffset_ = intProp "sticky-offset"
-----------------------------------------------------------------------------
-- | 'stickyTop_'
--
-- <>
--
-- // DefaultValue: false
-- sticky-top?: boolean
--
-- Declared on the `<list-item>` child node to control whether the node
-- will be sticky at the top.
--
stickyTop_ :: Bool -> Attribute action
stickyTop_ = boolProp "sticky-top"
-----------------------------------------------------------------------------
-- | 'stickyBottom_'
--
-- <>
--
-- // DefaultValue: false
-- sticky-bottom?: boolean
--
-- Declared on the `<list-item>` child node to control whether the node
-- will be sticky at the bottom.
--
stickyBottom_ :: Bool -> Attribute action
stickyBottom_ = boolProp "sticky-bottom"
-----------------------------------------------------------------------------
-- | 'initialScrollIndex_'
--
-- <>
--
-- // DefaultValue: 0
-- initial-scroll-index?: number
--
-- The offset distance from the top or bottom of `<list>` for sticky positioning, in `px`.
--
initialScrollIndex_ :: Int -> Attribute action
initialScrollIndex_ = intProp "initial-scroll-index"
-----------------------------------------------------------------------------
-- | 'needVisibleItemInfo_'
--
-- <>
--
-- // DefaultValue: false
-- need-visible-item-info?: boolean
--
-- Controls whether the scroll event callback parameters include the position
-- information of the currently rendering node.
--
-- The scroll events include:
--  * `scroll`
--  * `scrolltoupper`
--  * `scrolltolower`
--
needVisibleItemInfo_ :: Bool -> Attribute action
needVisibleItemInfo_ = boolProp "sticky-bottom"
-----------------------------------------------------------------------------
-- | 'upperThresholdItemCount_'
-- 
-- // DefaultValue: 0
-- upper-threshold-item-count?: number
-- 
-- Triggers a [`scrolltoupper`](#scrolltoupper) event once when the number of remaining displayable child nodes at the top of `<list>` is less than [`upper-threshold-item-count`](#upper-threshold-item-count) for the first time.
--
upperThresholdItemCount_ :: Int -> Attribute action
upperThresholdItemCount_ = intProp "upper-threshold-item-count"
-----------------------------------------------------------------------------
-- | 'lowerThresholdItemCount_'
-- 
-- // DefaultValue: 0
-- lower-threshold-item-count?: number
-- 
-- Triggers a [`scrolltolower`](#scrolltolower) event once when the number of remaining displayable child nodes at the bottom of `<list>` is less than [`lower-threshold-item-count`](#lower-threshold-item-count) for the first time.
--
lowerThresholdItemCount_ :: Int -> Attribute action
lowerThresholdItemCount_ = intProp "lower-threshold-item-count"
-----------------------------------------------------------------------------
-- | 'scrollEventThrottle_'
-- 
-- // DefaultValue: 200
-- scroll-event-throttle?: number
-- 
-- Sets the time interval for the `<list>` callback [`scroll`](#scroll) event, in milliseconds (ms). By default, the scroll event is called back every 200 ms.
--
scrollEventThrottle_ :: Int -> Attribute action
scrollEventThrottle_ = intProp "scroll-event-throttle"
-----------------------------------------------------------------------------
-- | 'layoutId_'
-- 
-- // defaultValue: -1
-- layout-id?: number
-- 
-- Used to mark the unique identifier for this data source update, which will be returned in the [`layoutcomplete`](#layoutcomplete) event callback.
--
layoutId_ :: Int -> Attribute action
layoutId_ = intProp "layout-id"
-----------------------------------------------------------------------------
-- ### `preload-buffer-count`
-- ``ts
-- // DefaultValue: 0
-- preload-buffer-count?: number
-- ```
-- 
-- This attribute controls the number of nodes outside `<list>` that are preloaded.
-- 
-- ### `scroll-bar-enable` <IOSOnly/>
-----------------------------------------------------------------------------
-- ### `full-span`
-- ``ts
-- // DefaultValue: false
-- full-span?: boolean
-- ```
-- 
-- The `full-span` attribute is used to indicate that a `<list-item>` occupies a full row or column.
-- 
-- ### `estimated-main-axis-size-px`
