-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.View.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.View.Event
  ( -- *** Event
    onTouchStart
  , onTouchMove
  , onTouchEnd
  , onTouchCancel
  , onTap
  , onLongPress
  , onLayoutChange
  , onAppear
  , onDisappear
  , onAnimationStart
  , onAnimationEnd
  , onAnimationCancel
  , onAnimationIteration
  , onTransitionStart
  , onTransitionEnd
  , onTransitionCancel
  ) where
-----------------------------------------------------------------------------
import           Miso.Types (Attribute)
import           Miso.Event (on, emptyDecoder)
-----------------------------------------------------------------------------
onTouchStart :: action -> Attribute action
onTouchStart action = on "touchstart" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onTouchMove :: action -> Attribute action
onTouchMove action = on "touchmove" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onTouchEnd :: action -> Attribute action
onTouchEnd action = on "touchend" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onTouchCancel :: action -> Attribute action
onTouchCancel action = on "touchcancel" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onTap :: action -> Attribute action
onTap action = on "tap" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onLongPress :: action -> Attribute action
onLongPress action = on "longpress" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onLayoutChange :: action -> Attribute action
onLayoutChange action = on "layoutchange" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onAppear :: action -> Attribute action
onAppear action = on "uiappear" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onDisappear :: action -> Attribute action
onDisappear action = on "uidisappear" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onAnimationStart :: action -> Attribute action
onAnimationStart action = on "animationstart" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onAnimationEnd :: action -> Attribute action
onAnimationEnd action = on "animationend" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onAnimationCancel :: action -> Attribute action
onAnimationCancel action = on "animationcancel" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onAnimationIteration :: action -> Attribute action
onAnimationIteration action = on "animationiteration" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onTransitionStart :: action -> Attribute action
onTransitionStart action = on "transitionstart" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onTransitionEnd :: action -> Attribute action
onTransitionEnd action = on "transitionend" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
onTransitionCancel :: action -> Attribute action
onTransitionCancel action = on "transitioncancel" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
