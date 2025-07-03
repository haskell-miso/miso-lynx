-----------------------------------------------------------------------------
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lynx.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Lynx.Event
  ( -- * Events
    lynxEvents
  ) where
----------------------------------------------------------------------------
import           Miso.Lynx.Element.Image.Event      (imageEvents)
import           Miso.Lynx.Element.List.Event       (listEvents)
import           Miso.Lynx.Element.ScrollView.Event (scrollViewEvents)
import           Miso.Lynx.Element.Text.Event       (textEvents)
import           Miso.Lynx.Element.View.Event       (viewEvents)
import           Miso.Event                           (Events)
----------------------------------------------------------------------------
lynxEvents :: Events
lynxEvents = mconcat
  [ imageEvents
  , listEvents
  , scrollViewEvents
  , textEvents
  , viewEvents
  ]
----------------------------------------------------------------------------
