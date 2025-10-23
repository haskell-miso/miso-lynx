-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lynx.Element.Frame.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Lynx.Element.Frame.Property
  ( -- *** Property
    src_
  ) where
----------------------------------------------------------------------------
import           Miso.Property
import           Miso.Types
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/frame.html#src
--
-- Sets the loading path for the \<frame\> resource.
--
-- > src_ "http://url-goes-here.com"
--
src_ :: MisoString -> Attribute action
src_ = textProp "src"
-----------------------------------------------------------------------------
