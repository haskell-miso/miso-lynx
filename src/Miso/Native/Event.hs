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
  ( -- * Events
    nativeEvents
  , onTap
  ) where
----------------------------------------------------------------------------
import Miso.Types (Attribute)
import Miso.Event (Events, emptyDecoder, on)
----------------------------------------------------------------------------
import qualified Data.Map.Strict as M
----------------------------------------------------------------------------
onTap :: action -> Attribute action
onTap action = on "tap" emptyDecoder $ \() _ -> action
----------------------------------------------------------------------------
nativeEvents :: Events
nativeEvents = M.fromList [ ("tap", False) ]
----------------------------------------------------------------------------
