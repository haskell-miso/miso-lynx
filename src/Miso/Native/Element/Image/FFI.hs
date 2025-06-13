-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Image.FFI
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Image.FFI
  ( -- *** Methods
    startAnimation
  , pauseAnimation
  , stopAnimation
  , resumeAnimation
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Native.FFI (invokeExec)
-----------------------------------------------------------------------------
import           Language.Javascript.JSaddle (JSM)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#startanimate>
--
-- Starts an animation at the ID selected
--
-- > startAnimation "someImageId"
--
startAnimation :: MisoString -> JSM ()
startAnimation selector = invokeExec selector "startAnimate"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#pauseanimation>
--
-- Pauses an animation at the ID selected
--
-- > pauseAnimation "someImageId"
--
pauseAnimation :: MisoString -> JSM ()
pauseAnimation selector = invokeExec selector "pauseAnimation"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#resumeanimation>
--
-- Resumes an animation at the ID selected
--
-- > resumeAnimation "someImageId"
--
resumeAnimation :: MisoString -> JSM ()
resumeAnimation selector = invokeExec selector "resumeAnimation"
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#stopanimation>
--
-- Stops an animation at the ID selected
--
-- > stopAnimation "someImageId"
--
stopAnimation :: MisoString -> JSM ()
stopAnimation selector = invokeExec selector "stopAnimation"
-----------------------------------------------------------------------------
