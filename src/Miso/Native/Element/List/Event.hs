-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.List.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.List.Event
  ( -- *** Event
    onScroll
  , onScrollToUpper
  , onScrollToLower
  , onScrollStateChange
  , onLayoutComplete
  , onSnap
  ) where
-----------------------------------------------------------------------------
import           Miso.Types (Attribute)
import           Miso.Event (on, emptyDecoder)
-----------------------------------------------------------------------------
onScroll :: action -> Attribute action
onScroll action = on "scroll" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onScrollToUpper :: action -> Attribute action
onScrollToUpper action = on "scrolltoupper" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onScrollToLower :: action -> Attribute action
onScrollToLower action = on "scrolltolower" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onScrollStateChange :: action -> Attribute action
onScrollStateChange action = on "scrollstatechange" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onLayoutComplete :: action -> Attribute action
onLayoutComplete action = on "layoutcomplete" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onSnap :: action -> Attribute action
onSnap action = on "snap" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
