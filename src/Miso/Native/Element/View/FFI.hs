-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
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
    let readProp = \name ->
          fromJSValUnchecked =<<
            rect ! (name :: MisoString)    
    x      <- readProp "x"
    y      <- readProp "y"
    height <- readProp "height"
    width  <- readProp "width"
    top    <- readProp "top"
    right  <- readProp "right"
    left   <- readProp "left"
    bottom <- readProp "bottom"
    pure $ Just Rect {..}
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
  selector_ <- toJSVal selector
  successful_ <- toJSVal =<< do
    syncCallback1 $ \arg -> do
      rect <- fromJSValUnchecked arg
      sink (successful rect)
  errorful_ <- toJSVal =<< do
    syncCallback1 $ \arg -> do
      rect <- fromJSValUnchecked arg
      sink (errorful rect)
  params <- create
  set "androidEnableTransformProps" androidEnableTransformProps params
  set "relativeTo" relativeTo params
  params_ <- toJSVal params
  method__ <- toJSVal ("boundingClientRect" :: MisoString)
  void $ do
    jsg ("globalThis" :: MisoString) # ("invokeExec" :: MisoString) $
      [ selector_
      , method__
      , params_
      , successful_
      , errorful_
      ]
-----------------------------------------------------------------------------
takeScreenshot :: JSM ()
takeScreenshot = undefined
-----------------------------------------------------------------------------
requestAccessibilityFocus :: JSM ()
requestAccessibilityFocus = undefined
-----------------------------------------------------------------------------
