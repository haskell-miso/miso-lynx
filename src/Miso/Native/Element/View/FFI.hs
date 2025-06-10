-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.View.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.View.FFI
  ( -- *** Methods
    boundingClientRect
  , takeScreenshot
  , requestAccessibilityFocus
  ) where
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle (JSM)
-----------------------------------------------------------------------------
boundingClientRect :: JSM ()
boundingClientRect = undefined
-----------------------------------------------------------------------------
takeScreenshot :: JSM ()
takeScreenshot = undefined
-----------------------------------------------------------------------------
requestAccessibilityFocus :: JSM ()
requestAccessibilityFocus = undefined
-----------------------------------------------------------------------------
