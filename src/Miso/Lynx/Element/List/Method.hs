-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lynx.Element.List.Method
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Lynx.Element.List.Method
  ( -- *** Methods
    scrollToPosition
  , autoScroll
  , getVisibleCells
  , scrollBy
  -- *** Types
  , ScrollToPosition (..)
  , AutoScroll (..)
  , ScrollBy (..)
  -- *** Smart constructors
  , defaultScrollToPosition
  , defaultAutoScroll
  , defaultScrollBy
  ) where
-----------------------------------------------------------------------------
import Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import Miso
import Miso.String
import Miso.Lynx.FFI
-----------------------------------------------------------------------------
data ScrollToPosition
  = ScrollToPosition
  { position :: Double
  -- ^ Specifies the index of the node to scroll to, with a range of [0, data source count)
  , offset :: Double
  -- ^ After applying alignTo alignment, continue scrolling the offset length
  , alignTo :: MisoString
  -- ^ The position of the target node in the view after scrolling. 
  , smooth :: Bool
  -- ^ Whether there is animation during the scrolling process
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Smart constructor for constructing 'scrollToPosition'
defaultScrollToPosition :: ScrollToPosition
defaultScrollToPosition
  = ScrollToPosition
  { position = 10
  , offset = 100
  , alignTo = "top"
  , smooth = True
  }
-----------------------------------------------------------------------------
instance ToJSVal ScrollToPosition where
  toJSVal ScrollToPosition {..} = do
    object <- create
    set "position" position object
    set "offset" offset object
    set "alignTo" alignTo object
    set "smooth" smooth object
    toJSVal object 
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/list.html#scrolltoposition
--
-- The front end can execute 'boundingClientRect' through the SelectorQuery API.
--
-- @
--
-- data Action
--   = Success MisoString
--   | Failure MisoString
--   | GetRect
--
-- update :: Action -> Effect model Action
-- update GetRect =
--   scrollToPosition defaultscrollToPosition "#box" Success Failure
-- update (Succes _) =
--   consoleLog "Successfuly got position"
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call scrollToPosition: " <> errorMsg)
--
-- @
--
scrollToPosition
  :: MisoString
  -> ScrollToPosition
  -> (MisoString -> action)
  -> (MisoString -> action)
  -> Effect model action
scrollToPosition = invokeExec "scrollToPosition"
--------------------------------------------------------------------
data AutoScroll
  = AutoScroll
  { rate :: MisoString
  , start :: Bool
  , autoStop :: Bool
  }
--------------------------------------------------------------------
instance ToJSVal AutoScroll where
  toJSVal AutoScroll {..} = do
    o <- create
    set "rate" rate o
    set "start" start o
    set "autoStop" autoStop o
    toJSVal o
--------------------------------------------------------------------
defaultAutoScroll :: AutoScroll
defaultAutoScroll = undefined
--------------------------------------------------------------------
autoScroll
  :: MisoString
  -> AutoScroll
  -> (MisoString -> action)
  -> (MisoString -> action)
  -> Effect model action
autoScroll = invokeExec "scrollToPosition"
--------------------------------------------------------------------
getVisibleCells
  :: MisoString
  -> (MisoString -> action)
  -> (MisoString -> action)
  -> Effect model action
getVisibleCells name = invokeExec "getVisibleCells" name ()
--------------------------------------------------------------------
data ScrollBy
  = ScrollBy
  { scrollByOffset :: Double
  }
--------------------------------------------------------------------
instance ToJSVal ScrollBy where
  toJSVal ScrollBy {..} = do
    o <- create
    set "offset" scrollByOffset o 
    toJSVal o
--------------------------------------------------------------------
defaultScrollBy :: ScrollBy
defaultScrollBy = ScrollBy 0
--------------------------------------------------------------------
scrollBy
  :: MisoString
  -> ScrollBy
  -> (Consumed -> action)
  -> (MisoString -> action)
  -> Effect model action
scrollBy = invokeExec "scrollBy"
--------------------------------------------------------------------
data Consumed
  = Consumed
  { consumedX, consumedY :: Double
  , unconsumedX, unconsumedY :: Double
  } deriving (Eq, Show)
--------------------------------------------------------------------
instance FromJSVal Consumed where
  fromJSVal o = do
    consumedX <- fromJSValUnchecked =<< o ! ("consumedX" :: MisoString)
    consumedY <- fromJSValUnchecked =<< o ! ("consumedY" :: MisoString)
    unconsumedX <- fromJSValUnchecked =<< o ! ("unconsumedX" :: MisoString)
    unconsumedY <- fromJSValUnchecked =<< o ! ("unconsumedY" :: MisoString)
    pure $ Just Consumed {..}
--------------------------------------------------------------------
