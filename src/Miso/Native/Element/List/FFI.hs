-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.List.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.List.FFI
  ( -- *** Property
    scrollToPosition
--  , autoScroll
  , getVisibleCells
--  , scrollBy
  ) where
-----------------------------------------------------------------------------
-- import           Miso.String (MisoString)
-- import           Miso.Html (View, Attribute, node, NS(NATIVE))
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle (JSM)
-----------------------------------------------------------------------------
scrollToPosition :: JSM ()
scrollToPosition = undefined
-----------------------------------------------------------------------------
-- autoScroll :: JSM ()
-- autoScroll = undefined
-----------------------------------------------------------------------------
getVisibleCells :: JSM ()
getVisibleCells = undefined
-----------------------------------------------------------------------------
-- scrollBy :: JSM ()
-- scrollBy = undefined
-----------------------------------------------------------------------------
