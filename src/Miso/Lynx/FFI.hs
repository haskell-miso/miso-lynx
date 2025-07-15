-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lynx.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Lynx.FFI
  ( -- *** Lynx specific FFI
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
import Miso
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
invokeExec
  :: (ToJSVal params, FromJSVal argument)
  => MisoString
  -- ^ method
  -> MisoString
  -- ^ selector
  -> params
  -- ^ params
  -> (argument -> action)
  -- ^ successful
  -> (MisoString -> action)
  -- ^ errorful
  -> Effect model action
invokeExec method selector params successful errorful = do
  withSink $ \sink -> do
    selector_ <- toJSVal selector
    successful_ <- toJSVal =<< do
      asyncCallback1 $ \arg -> do
        result <- fromJSValUnchecked arg
        sink (successful result)
    errorful_ <- toJSVal =<< do
      asyncCallback1 $ \arg -> do
        rect <- fromJSValUnchecked arg
        sink (errorful rect)
    params_ <- toJSVal params
    method__ <- toJSVal method
    void $ do
      jsg (ms "globalThis" :: MisoString) # (ms "invokeExec" :: MisoString) $
        [ selector_
        , method__
        , params_
        , successful_
        , errorful_
        ]
-----------------------------------------------------------------------------
