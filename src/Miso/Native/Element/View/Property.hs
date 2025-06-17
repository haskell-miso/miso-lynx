-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.View.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.View.Property
  ( -- *** Property
    name_
  , class_
  , className_
  , flatten_
  , exposureId_
  , exposureArea_
  , exposureMargin_
  , accessibilityElement_
  , accessibilityLabel_
  , exposureScene_
  , accessibilityTrait_
  , accessibilityElements_
  , accessibilityElementsA11y_
  , accessibilityElementsHidden_
  , accessibilityExclusiveFocus_
  , a11yId_
  , iosPlatformAccessibilityId_
  , userInteractionEnabled_
  , nativeInteractionEnabled_
  , blockNativeEvent_
  , enableTouchPseudoPropagation_
  , hitSlop_
  , ignoreFocus_
  , eventThrough_
  , iosEnableSimultaneousTouch_
  , timingFlag_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Property
import           Miso.Types 
-----------------------------------------------------------------------------
-- | 'name_'
-- 
-- // DefaultValue undefined
-- name?: string
-- 
-- Used to specify the name of the element, generally for native to operate the corresponding node from the native side through `findViewByName`.
-- 
-- 
name_ :: MisoString -> Attribute action
name_ = textProp "name"
-----------------------------------------------------------------------------
-- | 'class_'
-- 
-- class?: string;
-- 
-- Used to specify one or more class names for an element, which can be used in CSS to apply styles.
-- 
class_ :: MisoString -> Attribute action
class_ = textProp "class_"
-----------------------------------------------------------------------------
-- | 'className_' <Badge text="ReactLynx" />
-- 
-- ```ts
-- className?: string;
-- ```
-- 
-- In ReactLynx, use `className` to set CSS class names, equivalent to [`class`](#class).
-- 
-- ### `data-*`
-- 
-- 
className_ :: MisoString -> Attribute action
className_ = textProp "className_"
-----------------------------------------------------------------------------
--  `data-*`
-- |
-- ''_'ts
-- data-*?: any;
-- ```
-- 
-- Used to specify additional information for the element, which can be obtained in [Event](../../lynx-api/event/event.mdx#target).
-- 
-- ### `flatten` <AndroidOnly/>
-- 
-- data-*`
-- |
-- ''_ :: MisoString -> Attribute action
-- ''_ :: MisoString  = textProp -- ''_ :: "MisoString "
-- |
-----------------------------------------------------------------------------
-- | 'flatten_' AndroidOnly
-- 
-- flatten?: boolean;
-- 
-- Only available on Android platform, used to force specific nodes to create corresponding Android Views.
-- 
flatten_ :: Bool -> Attribute action
flatten_ = boolProp "flatten"
-----------------------------------------------------------------------------
-- | 'exposureId_'
-- 
-- // DefaultValue: undefined
-- exposure-id?: string
-- 
-- Specify whether the target node needs to listen to [exposure/anti-exposure events](../../../guide/interaction/visibility-detection/exposure-ability.mdx#monitor-exposure-of-the-entire-page).
-- 
exposureId_ :: MisoString -> Attribute action
exposureId_ = textProp "exposureId_"
-----------------------------------------------------------------------------
-- | 'exposure-scene_'
-- 
-- // DefaultValue: undefined
-- exposure-scene?: string
-- 
-- Specify the exposure scene of the target node, and use it together with [`exposure-id`](#exposure-id) to uniquely identify the node that needs to monitor exposure.
-- 
-- 
exposureScene_ :: MisoString -> Attribute action
exposureScene_ = textProp "exposure-scene"
-----------------------------------------------------------------------------
-- | 'exposure-ui-margin-*_'
-- 
-- // DefaultValue: '0px'
-- exposure-ui-margin-top?: string;
-- exposure-ui-margin-right?: string;
-- exposure-ui-margin-bottom?: string;
-- exposure-ui-margin-left?: string;
-- 
-- 
-- margin-*_ :: MisoString -> Attribute action
-- margin- = textProp "margin-"
-----------------------------------------------------------------------------
-- | 'exposure-screen-margin-*_'
-- 
-- ```ts
-- // DefaultValue: '0px'
-- exposure-screen-margin-top?: string;
-- exposure-screen-margin-right?: string;
-- exposure-screen-margin-bottom?: string;
-- exposure-screen-margin-left?: string;
-- ```
-- 
-- 
-- margin-*_ :: MisoString -> Attribute action
-- margin- = textProp "margin-"
-----------------------------------------------------------------------------
-- | 'exposureArea_'
-- 
-- ```ts
-- // DefaultValue: '0%'
-- exposure-area?: string
-- ```
-- 
-- Specify the viewport intersection ratio of the target node that can trigger the exposure event. When it is greater than this ratio, the exposure event is triggered. When it is less than this ratio, the reverse exposure event is triggered. By default, the exposure event is triggered when the target node is exposed.
-- 
exposureArea_ :: MisoString -> Attribute action
exposureArea_ = textProp "exposure-area"
-----------------------------------------------------------------------------
-- | 'exposureMargin_'
--
-- ```ts
-- // DefaultValue: false
-- enable-exposure-ui-margin?: boolean
-- ``` 
--
-- Specify whether the target node supports the [`exposure-ui-margin-*`](#exposure-ui-margin-) properties.
--
-- Setting it to `true` will change the behavior of [`exposure-screen-margin-*`](#exposure-screen-margin-)
-- and may cause the lazy loading.
--
exposureMargin_ :: Bool -> Attribute action
exposureMargin_ = boolProp "exposure-margin"
-----------------------------------------------------------------------------
-- | 'accessibilityElement_'
-- 
-- // DefaultValue: image and text nodes are true by default, and other nodes are false by default
-- accessibility-element?: boolean
-- 
-- Set whether the node supports accessibility.
-- 
accessibilityElement_ :: Bool -> Attribute action
accessibilityElement_ = boolProp "accessibility-element"
-----------------------------------------------------------------------------
-- | 'accessibilityLabel_'
-- 
-- // DefaultValue: undefined
-- accessibility-label?: string
-- 
-- Set the content of the node voice broadcast.
-- 
-- If the `<text/>` node does not set this attribute, the `<text/>` node defaults to the `<text/>` content.
-- 
accessibilityLabel_ :: MisoString -> Attribute action
accessibilityLabel_ = textProp "accessibility-label"
-----------------------------------------------------------------------------
-- | 'accessibilityTrait_'
-- 
-- // DefaultValue: "none"
-- accessibility-traits?: "none" | "button" | "image" | "text"
-- 
-- Set the type characteristics of the node. The system will have specific supplements to the playback content for different types of nodes.
-- 
accessibilityTrait_ :: MisoString -> Attribute action
accessibilityTrait_ = textProp "accessibility-trait"
-----------------------------------------------------------------------------
-- | 'accessibility-elements_'
-- 
-- // DefaultValue: undefined
-- accessibility-elements?: string
-- 
-- Customize the focus order of child nodes. This property is set on the parent node, and the focus order of its child nodes will be focused according to the order of the child node `id` specified by the `accessibility-elements` property.
-- 
accessibilityElements_ :: MisoString -> Attribute action
accessibilityElements_ = textProp "accessibilityElements_"
-----------------------------------------------------------------------------
-- | 'accessibilityElementsA11y_'
-- 
-- ```ts
-- // DefaultValue: undefined
-- accessibility-elements-a11y?: string
-- ```
-- 
-- The same as `accessibility-elements`, but the corresponding `id` is `a11y-id`.
-- 
accessibilityElementsA11y_ :: MisoString -> Attribute action
accessibilityElementsA11y_ = textProp "accessibility-elements-a11y"
-----------------------------------------------------------------------------
-- | 'accessibilityElementsHidden_'
-- 
-- ```ts
-- // DefaultValue: false
-- accessibility-elements-hidden?: boolean
-- ```
-- 
-- Marks the current node and all its child nodes as non-accessible nodes.
-- 
-- ### `accessibility-exclusive-focus`
-- 
accessibilityElementsHidden_ :: Bool -> Attribute action
accessibilityElementsHidden_ = boolProp "accessibilityElementsHidden_"
-----------------------------------------------------------------------------
-- | 'accessibility-exclusive-focus_'
-- 
-- ```ts
-- // DefaultValue: false
-- accessibility-exclusive-focus?: boolean
-- ```
-- 
-- This property can be set for any node. In accessibility mode, sequential navigation will only focus on the child nodes under these nodes.
-- 
-- 
accessibilityExclusiveFocus_ :: Bool -> Attribute action
accessibilityExclusiveFocus_ = boolProp "accessibility-exclusive-focus"
-----------------------------------------------------------------------------
-- | 'a11yId_'
-- 
-- ```ts
-- // DefaultValue: undefined
-- a11y-id?: string
-- ```
-- 
-- Different from `id`, it is used to identify barrier-free nodes separately.
-- 
-- ### `ios-platform-accessibility-id`
-- 
a11yId_ :: MisoString -> Attribute action
a11yId_ = textProp "a11y-id"
-----------------------------------------------------------------------------
-- | 'ios-platform-accessibility-id_'
-- 
-- ```ts
-- // DefaultValue: undefined
-- ios-platform-accessibility-id?: string
-- ```
-- 
-- Used to specify the accessibility identifier of a `UIView` in iOS. It is
-- only used when the platform-level accessibility framework is accessed.
-- 
-- 
iosPlatformAccessibilityId_ :: MisoString -> Attribute action
iosPlatformAccessibilityId_ = textProp "ios-platform-accessibility-id"
-----------------------------------------------------------------------------
-- | 'userInteractionEnabled_'
-- 
-- ```ts
-- // DefaultValue: true
-- user-interaction-enabled?: boolean
-- ```
-- 
-- Specifies whether the target node and its child nodes can respond to Lynx touch events. This property does not affect platform-level gestures (such as scrolling of `scroll-view`).
-- 
-- 
userInteractionEnabled_ :: Bool -> Attribute action
userInteractionEnabled_ = boolProp "user-interaction-enabled"
-----------------------------------------------------------------------------
-- | 'native-interaction-enabled_'
-- 
-- ```ts
-- // DefaultValue: true for iOS, false for Android
-- native-interaction-enabled?: boolean
-- ```
-- 
-- Specify whether the target node consumes platform-layer touch events, affects platform-layer gestures (such as scrolling of `scroll-view`), does not affect Lynx touch events, and can achieve similar platform-layer gesture penetration/interception effects.
-- 
nativeInteractionEnabled_ :: Bool -> Attribute action
nativeInteractionEnabled_ = boolProp "native-interaction-enabled"
-----------------------------------------------------------------------------
-- | 'blockNativeEvent_'
-- 
-- ```ts
-- // DefaultValue: false
-- block-native-event?: boolean
-- ```
-- 
-- Specify whether to block platform layer gestures outside Lynx when the target node is on the [event response chain](../../../guide/interaction/event-handling/event-propagation.mdx#event-response-chain), which can achieve an effect similar to blocking the platform layer side sliding back.
-- 
blockNativeEvent_ :: Bool -> Attribute action
blockNativeEvent_ = boolProp "block-native-event"
-----------------------------------------------------------------------------
-- | 'eventThrough_'
--
-- // DefaultValue: false
-- event-through?: boolean
-- ```
-- Specifies whether the touch event of the platform layer is distributed to Lynx when the touch is on the target node, which can achieve a similar effect of only displaying without interaction. This property supports inheritance.
-- 
eventThrough_ :: Bool -> Attribute action
eventThrough_ = boolProp "event-through"
-----------------------------------------------------------------------------
-- | 'enableTouchPseudoPropagation_'
--
-- // DefaultValue: false
-- enable-touch-pseudo-propagation?: boolean
--
-- Specify whether the target node supports the `:active` pseudo-class to continue bubbling up on the [event response chain](../../../guide/interaction/event-handling/event-propagation.mdx#event-response-chain).
-- 
enableTouchPseudoPropagation_ :: MisoString -> Attribute action
enableTouchPseudoPropagation_ = textProp "enable-touch-pseudo-propagation"
-----------------------------------------------------------------------------
-- | 'hitSlop_'
-- 
-- // DefaultValue: '0px' or {top: '0px', left: '0px', right: '0px', bottom: '0px'}
-- hit-slop?: object | string
--
-- Specify the touch event response hotspot of the target node, without affecting the platform layer gesture.
-- 
hitSlop_ :: MisoString -> Attribute action
hitSlop_ = textProp "hit-slop"
-----------------------------------------------------------------------------
-- | 'ignoreFocus_'
-- 
-- // DefaultValue: false
-- ignore-focus?: boolean
-- 
-- Specify whether to not grab focus when touching the target node. By default, the node grabs focus when clicking on it, which can achieve a similar effect of not closing the keyboard when clicking other areas.
-- 
ignoreFocus_ :: MisoString -> Attribute action
ignoreFocus_ = textProp "ignore-focus"
-----------------------------------------------------------------------------
-- | `ios-enable-simultaneous-touch_' <IOSOnly />
-- 
-- // DefaultValue: false-- 
-- ios-enable-simultaneous-touch?: boolean-- 
--
iosEnableSimultaneousTouch_ :: Bool -> Attribute action
iosEnableSimultaneousTouch_ = boolProp "ios-enable-simultaneous-touch"
-----------------------------------------------------------------------------
-- | 'timingFlag_'
-- 
-- // DefaultValue: '0px' or {top: '0px', left: '0px', right: '0px', bottom: '0px'}
-- hit-slop?: object | string
--
-- Specify the touch event response hotspot of the target node, without affecting the platform layer gesture.
-- 
timingFlag_ :: MisoString -> Attribute action
timingFlag_ = textProp "__lynx_timing_flag"
-----------------------------------------------------------------------------
