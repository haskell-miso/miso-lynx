-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Event
  ( onTap
  ) where
----------------------------------------------------------------------------
import Miso.Html (Attribute, on)
import Miso.Event
----------------------------------------------------------------------------
onTap :: action -> Attribute action
onTap action = on "tap" emptyDecoder $ \() -> action
----------------------------------------------------------------------------
