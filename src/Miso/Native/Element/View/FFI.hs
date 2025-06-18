{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
-----------------------------------------------------------------------------
{-# LANGUAGE ScopedTypeVariables #-}
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
  -- *** Types
  , Rect (..)
  , BoundingClientRect (..)
  -- *** Smart constructors
  , defaultBoundingClientRect
  ) where
-----------------------------------------------------------------------------
import Control.Monad
import Language.Javascript.JSaddle
-----------------------------------------------------------------------------
import Miso
import Miso.String
-----------------------------------------------------------------------------
-- | Result of calling `getClientBoundingRect`
data Rect
  = Rect
  { x,y :: Double
  , width, height :: Double
  , top, bottom :: Double
  , right, left :: Double
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSVal Rect where
  fromJSVal = \rect -> do
    x <- readProp rect "x"
    y <- readProp rect "y"
    width <- readProp rect "width"
    height <- readProp rect "height"
    top <- readProp rect "top"
    right <- readProp rect "right"
    bottom <- readProp rect "bottom"
    left <- readProp rect "left"
    pure $ Just Rect {..}
      where
        readProp rect name =
          fromJSValUnchecked =<<
            rect ! (name :: MisoString)    
-----------------------------------------------------------------------------
data BoundingClientRect action
  = BoundingClientRect
  { successful :: Rect -> action
  -- ^ Successful callback
  , errorful :: MisoString -> action
  -- ^ Errorful callback
  , selector :: MisoString
  -- ^ Selector identifier (e.g. "#box")
  , androidEnableTransformProps :: Bool
  -- ^ Specifies whether to consider the transform attribute
  -- when calculating the position on Android. The default value is 'False'
  , relativeTo :: Maybe JSVal
  -- ^ Specify the reference node, relative to LynxView by default.
  }
-----------------------------------------------------------------------------
-- | Smart constructor for calling 'boundingClientRect'
defaultBoundingClientRect
  :: MisoString
  -- ^ Selector (e.g. "#box")
  -> (Rect -> action)
  -- ^ Successful callback (contains 'Rect')
  -> (MisoString -> action)
  -- ^ Errorful callback, contains error message
  -> BoundingClientRect action
defaultBoundingClientRect selector successful errorful
  = BoundingClientRect
  { androidEnableTransformProps = True
  , relativeTo = Nothing
  , ..
  }
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#boundingclientrect
--
-- The front end can execute 'boundingClientRect' through the SelectorQuery API.
--
-- @
--
-- data Action
--   = Success Rect
--   | Failure MisoString
--   | GetRect
--
-- update :: Action -> Effect model Action
-- update GetRect = boundingClientRect
--   (defaultBoundingClientRect "#box" Success Failure)
-- update (Succes Rect {..}) =
--   consoleLog "Successfuly got Rect"
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call getClientBoundingRect: " <> errorMsg)
--
-- @
--
boundingClientRect
  :: BoundingClientRect action
  -> Effect model action
boundingClientRect BoundingClientRect {..} = withSink $ \sink -> do
  lynx <- jsg @MisoString "lynx"
  query <- lynx # ("createSelectorQuery" :: MisoString) $ ([] :: [JSVal])
  n <- query # ("select" :: MisoString) $ selector
  object <- create
  params <- create
  set "androidEnableTransformProps" androidEnableTransformProps params
  set "relativeTo" relativeTo params
  set "params" params object
  set @MisoString "method" "boundingClientRect" object
  flip (set "success") object =<< do
    syncCallback1 $ \arg -> do
      rect <- fromJSValUnchecked arg
      sink (successful rect)
  flip (set "error") object =<< do
    syncCallback1 $ \arg -> do
      rect <- fromJSValUnchecked arg
      sink (errorful rect)
  method <- n # ("invoke" :: MisoString) =<< toJSVal object
  void $ method # ("exec" :: MisoString) $ ([] :: [MisoString])
-----------------------------------------------------------------------------
takeScreenshot :: JSM ()
takeScreenshot = undefined
-----------------------------------------------------------------------------
requestAccessibilityFocus :: JSM ()
requestAccessibilityFocus = undefined
-----------------------------------------------------------------------------
