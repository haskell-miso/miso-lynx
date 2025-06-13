-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.FFI
  ( -- *** Native specific FFI
    setInterval
  , clearInterval
  , setTimeout
  , clearTimeout
  , invokeExec
  ) where
----------------------------------------------------------------------------
import Control.Monad
import Language.Javascript.JSaddle
----------------------------------------------------------------------------
import Miso.FFI
import Miso.String
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/lynx-api/global/set-interval.html>
--
setInterval :: Double -> JSM () -> JSM Double
setInterval delay f = do
  cb <- toJSVal =<< asyncCallback f
  v <- toJSVal delay
  result <- jsg "lynx" # "setInterval" $ ([cb, v] :: [JSVal])
  fromJSValUnchecked result
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/lynx-api/global/clear-interval.html>
--
clearInterval :: Double -> JSM Double
clearInterval intervalId = do
  result <- jsg "lynx" # "clearInterval" $ [intervalId]
  fromJSValUnchecked result
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/lynx-api/global/set-timeout.html>
--
setTimeout :: Double -> JSM () -> JSM Double
setTimeout delay f = do
  cb <- toJSVal (asyncCallback f)
  v <- toJSVal delay
  result <- jsg "lynx" # "setTimeout" $ [cb, v]
  fromJSValUnchecked result
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/lynx-api/global/clear-timeout.html>
--
clearTimeout :: Double -> JSM ()
clearTimeout timerId = void $ jsg "lynx" # "clearTimeout" $ [timerId]
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/lynx-api/nodes-ref/nodes-ref-invoke.html>
--
-- Used to call methods on elements in 'view_', 'image_', etc.
-- We use this internally to implement the various 'Method' sections
-- per the lynx docs.
--
-- > invokeExec "gifs" "startAnimate" :: JSM ()
--
-- @ 
-- lynx.createSelectorQuery()
--   .select('#gifs')
--   .invoke({
--    method: 'startAnimate'，
--  }).exec();
-- @
--
--
-- > invoke
--
invokeExec :: MisoString -> MisoString -> JSM ()
invokeExec elementId params = do
  params_ <- toJSVal params
  elementId_ <- toJSVal elementId
  void
    $ jsg1 "invokeExec"
    $ [elementId_, params_]
-----------------------------------------------------------------------------
