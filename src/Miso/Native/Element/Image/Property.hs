-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Image.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Image.Property
  ( -- *** Property
    mode_
  , placeholder_
  , blurRadius_
  , prefetchWidthHeight_
  , capInsets_
  , capInsetsScale_
  , loopCount_
  , imageConfig_
  , autoSize_
  , deferSrcInvalidation_
  , autoPlay_
  , tintColor_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | 'mode_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#mode>
--
-- DefaultValue: 'scaleToFill'
--
-- Specifies the image cropping/scaling mode
--
-- mode?: 'scaleToFill' | 'aspectFit' | 'aspectFill';
--
-- > mode_ "https://<your-domain-here>/image.png"
--
mode_ :: MisoString -> Attribute action
mode_ = textProp "mode"
-----------------------------------------------------------------------------
-- | 'placeholder_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#placeholder>
--
-- Specifies the path to the placeholder image. The usage and limitations are the same as for the `src` attribute.
--
-- > placeholder_ "value"
--
placeholder_ :: MisoString -> Attribute action
placeholder_ = textProp "placeholder"
-----------------------------------------------------------------------------
-- | 'blurRadius_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#blur-radius>
--
-- Specifies the Gaussian blur radius for the image.
--
-- // DefaultValue: '0px'
-- blur-radius?: string;
--
-- > image_ [ blurRadius_ "10px" ]
--
blurRadius_ :: MisoString -> Attribute action
blurRadius_ = textProp "blur-radius"
-----------------------------------------------------------------------------
-- | 'prefetchWidthHeight_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#prefetch-widthprefetch-height>
--
-- > image_ [ prefetchWidthHeight_ "value" ]
--
prefetchWidthHeight_ :: MisoString -> Attribute action
prefetchWidthHeight_ = textProp "prefetch-width/prefetch-height"
-----------------------------------------------------------------------------
-- | 'capInsets_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#cap-insets>
--
-- // DefaultValue: '0px 0px 0px 0px'
-- cap-insets?: string;
--
-- Specifies the 9patch image scaling area with four values representing the top, right, bottom, and left edges. Values must be specific numbers and do not support percentages or decimals.
--
-- > image_ [ capInsets_ "0px 14px 0 14px" ]
--
capInsets_ :: MisoString -> Attribute action
capInsets_ = textProp "cap-insets"
-----------------------------------------------------------------------------
-- | 'capInsetsScale_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#cap-insets-scale>
--
-- // DefaultValue: 1
-- cap-insets-scale?: number;
--
-- Works with `cap-insets` to adjust the pixel positions when stretching the image.
--
-- > image_ [ capInsetsScale_ 10 ]
--
capInsetsScale_ :: Int -> Attribute action
capInsetsScale_ = intProp "cap-insets-scale"
-----------------------------------------------------------------------------
-- | 'loopCount_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#loop-count>
--
-- // DefaultValue: 0
-- loop-count?: number;
--
-- Specifies the number of times to play an animated image. The default is to loop indefinitely.
--
-- > image_ [ loopCount_ 10 ]
--
loopCount_ :: Int -> Attribute action
loopCount_ = intProp "loop-count"
-----------------------------------------------------------------------------
-- | 'imageConfig_'
--
-- Android only.
--
-- <https://lynxjs.org/api/elements/built-in/image.html#image-config>
--
-- // DefaultValue: 'ARGB_8888'
-- image-config?: 'RGB_565' | 'ARGB_8888';
--
-- Specifies the number of times to play an animated image. The default is to loop indefinitely.
--
-- > image_ [ imageConfig_ "ARGB_8888" ]
--
imageConfig_ :: Int -> Attribute action
imageConfig_ = intProp "image-config"
-----------------------------------------------------------------------------
-- | 'autoSize_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#auto-size>
--
-- // DefaultValue: false
-- auto-size?: boolean;
--
-- When set to true and the <image> element has no width or height, the size of the <image> will be automatically adjusted to match the image's original dimensions after the image is successfully loaded, ensuring that the aspect ratio is maintained.
--
-- > image_ [ autoSize_ True ]
--
autoSize_ :: Bool -> Attribute action
autoSize_ = boolProp "auto-size"
-----------------------------------------------------------------------------
-- | 'deferSrcInvalidation_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#defer-src-invalidation>
--
-- // DefaultValue: false
-- defer-src-invalidation?: boolean;
--
-- Specifies the number of times to play an animated image. The default is to loop indefinitely.
--
-- > image_ [ deferSrcInvalidation_ True ]
--
deferSrcInvalidation_ :: Bool -> Attribute action
deferSrcInvalidation_ = boolProp "defer-src-invalidation"
-----------------------------------------------------------------------------
-- | 'autoPlay_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#auto-play>
--
-- // DefaultValue: true
-- auto-play?: boolean;
--
-- Specifies the number of times to play an animated image. The default is to loop indefinitely.
--
-- > image_ [ autoPlay_ False ]
--
autoPlay_ :: Bool -> Attribute action
autoPlay_ = boolProp "auto-play"
-----------------------------------------------------------------------------
-- | 'tintColor_'
--
-- <https://lynxjs.org/api/elements/built-in/image.html#tint-color>
--
-- // DefaultValue: 0
-- tint-color?: number;
--
-- Specifies the number of times to play an animated image. The default is to loop indefinitely.
--
-- > image_ [ tintColor_ 10 ]
--
tintColor_ :: Int -> Attribute action
tintColor_ = intProp "tint-color"
-----------------------------------------------------------------------------
