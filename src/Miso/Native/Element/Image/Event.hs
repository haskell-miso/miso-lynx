-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Image.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Image.Event
  ( -- *** Events
    onBindLoad
  , onBindError
  -- *** Decoder
  , imageLoadDecoder
  , imageErrorDecoder
  -- *** Types
  , ImageErrorEvent (..)
  , ImageLoadEvent (..)
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import           Miso.Event
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#bindload>
--
onBindLoad :: (ImageLoadEvent -> action) -> Attribute action
onBindLoad action = on "bindload" imageLoadDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | <https://lynxjs.org/api/elements/built-in/image.html#binderror>
--
onBindError :: (ImageErrorEvent -> action) -> Attribute action
onBindError action = on "binderror" imageErrorDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
-- | Callback when an 'image_' fails to load
data ImageErrorEvent
  = ImageErrorEvent
  { errMsg :: MisoString
    -- ^ error message
  , errorCode :: Int
    -- ^ error code
  , categorizedCode :: Int
    -- ^ lynx specific error code
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Callback when an 'image_' succeeds in loading
data ImageLoadEvent
  = ImageLoadEvent
  { width :: Int
    -- ^ 'image_' width
  , height :: Int
    -- ^ 'image_' height
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
imageLoadDecoder :: Decoder ImageLoadEvent
imageLoadDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      ImageLoadEvent
        <$> o .: "width"
        <*> o .: "height"
-----------------------------------------------------------------------------
imageErrorDecoder :: Decoder ImageErrorEvent
imageErrorDecoder = ["detail"] `at` details
  where
    details = withObject "detail" $ \o ->
      ImageErrorEvent
        <$> o .: "errMsg"
        <*> o .: "error_code"
        <*> o .: "lynx_categorized_code"
-----------------------------------------------------------------------------
