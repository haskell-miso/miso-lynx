-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.ScrollView.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.ScrollView.Event
  ( -- *** Event
    onBindScroll
  , onBindScrollToUpper
  , onBindScrollToLower
  , onBindScrollEnd
  , onBindContentSizeChanged
  ) where
-----------------------------------------------------------------------------
import           Miso.Types (Attribute)
import           Miso.Event
-----------------------------------------------------------------------------
onBindScroll :: action -> Attribute action
onBindScroll action = on "bindscroll" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onBindScrollToUpper :: action -> Attribute action
onBindScrollToUpper action = on "bindscrolltoupper" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onBindScrollToLower :: action -> Attribute action
onBindScrollToLower action = on "bindscrolltolower" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onBindScrollEnd :: action -> Attribute action
onBindScrollEnd action = on "bindscrollend" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
onBindContentSizeChanged :: action -> Attribute action
onBindContentSizeChanged action = on "bindcontentsizechanged" emptyDecoder (\_ _ -> action)
-----------------------------------------------------------------------------
