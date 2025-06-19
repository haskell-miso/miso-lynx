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
  , TakeScreenshot (..)
  -- *** Smart constructors
  , defaultBoundingClientRect
  , defaultTakeScreenshot
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
data BoundingClientRect
  = BoundingClientRect
  { androidEnableTransformProps :: Bool
  -- ^ Specifies whether to consider the transform attribute
  -- when calculating the position on Android. The default value is 'False'
  , relativeTo :: Maybe JSVal
  -- ^ Specify the reference node, relative to LynxView by default.
  }
-----------------------------------------------------------------------------
-- | Smart constructor for constructing 'boundingClientRect'
defaultBoundingClientRect :: BoundingClientRect
defaultBoundingClientRect
  = BoundingClientRect
  { androidEnableTransformProps = True
  , relativeTo = Nothing
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
-- update GetRect =
--   boundingClientRect defaultBoundingClientRect "#box" Success Failure
-- update (Succes Rect {..}) =
--   consoleLog "Successfuly got Rect"
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call getClientBoundingRect: " <> errorMsg)
--
-- @
--
boundingClientRect
  :: BoundingClientRect
  -> MisoString
  -> (Rect -> action)
  -> (MisoString -> action)
  -> Effect model action
boundingClientRect BoundingClientRect {..} selector successful errorful =
  withSink $ \sink -> do
    selector_ <- toJSVal selector
    successful_ <- toJSVal =<< do
      asyncCallback1 $ \arg -> do
        rect <- fromJSValUnchecked arg
        sink (successful rect)
    errorful_ <- toJSVal =<< do
      asyncCallback1 $ \arg -> do
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
data TakeScreenshot
  = TakeScreenshot
  { format_ :: MisoString 
  -- ^ e.g. Specify the image format, supports jpeg and png, the default is jpeg
  , scale_ :: Double
  -- ^ e.g. Specify the image quality, 0 < scale <= 1, the default is 1,
  -- the smaller the value, the blurrier and smaller the size.
  }
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#takescreenshot
--
-- The front end can execute 'takeScreenshot' through the SelectorQuery API.
--
-- @
--
-- data Action
--   = Success Image
--   | Failure MisoString
--   | GetScreenshot
--
-- update :: Action -> Effect model Action
-- update GetScreenshot = takeScreenshot
--   defaultTakeScreenshot "#my-view" Success Failure
-- update (Succes image) =
--   consoleLog "Successfuly got image"
--   consoleLog' image
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call takeScreenshot: " <> errorMsg)
--
-- @
--
takeScreenshot
  :: TakeScreenshot
  -> MisoString
  -> (Image -> action)
  -> (MisoString -> action)
  -> Effect model action
takeScreenshot TakeScreenshot {..} selector successful errorful = withSink $ \sink -> do
  selector_ <- toJSVal selector
  successful_ <- toJSVal =<< do
    asyncCallback1 $ \arg -> do
      sink (successful (Image arg))
  errorful_ <- toJSVal =<< do
    asyncCallback1 $ \arg -> do
      rect <- fromJSValUnchecked arg
      sink (errorful rect)
  params <- create
  set "scale" scale_ params
  set "format" format_ params
  params_ <- toJSVal params
  method <- toJSVal ("takeScreenshot" :: MisoString)
  void $ do
    jsg ("globalThis" :: MisoString) # ("invokeExec" :: MisoString) $
      [ selector_
      , method
      , params_
      , successful_
      , errorful_
      ]
-----------------------------------------------------------------------------
-- | Smart constructor for calling 'TakeScreenshot'
defaultTakeScreenshot :: TakeScreenshot
defaultTakeScreenshot
  = TakeScreenshot
  { scale_ = 0.5
  , format_ = ".png"
  }
-----------------------------------------------------------------------------
-- | https://lynxjs.org/api/elements/built-in/view.html#requestaccessibilityfocus
--
-- The front end can execute 'requestAccessiblityFocus' through the SelectorQuery API.
--
-- @
--
-- data Action
--   = Success
--   | Failure MisoString
--   | GetFocus
--
-- update :: Action -> Effect model Action
-- update GetFocus = requestAccessibilityFocus "#my-view" Success Failure
-- update Success = consoleLog "Successfuly got focus"
-- update (Failure errorMsg) =
--   consoleLog ("Failed to call requestAccessibilityFocus: " <> errorMsg)
--
-- @
--
requestAccessibilityFocus
  :: MisoString
  -> (Image -> action)
  -> (MisoString -> action)
  -> Effect model action
requestAccessibilityFocus selector successful errorful = withSink $ \sink -> do
  selector_ <- toJSVal selector
  successful_ <- toJSVal =<< do
    asyncCallback1 $ \arg -> do
      sink (successful (Image arg))
  errorful_ <- toJSVal =<< do
    asyncCallback1 $ \arg -> do
      error_ <- fromJSValUnchecked arg
      sink (errorful error_)
  params_ <- toJSVal =<< create
  method <- toJSVal ("requestAccessibilityFocus" :: MisoString)
  void $ do
    jsg ("globalThis" :: MisoString) # ("invokeExec" :: MisoString) $
      [ selector_
      , method
      , params_
      , successful_
      , errorful_
      ]
-----------------------------------------------------------------------------
