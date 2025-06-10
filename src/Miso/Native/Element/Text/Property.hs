-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Text.Property
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Text.Property
  ( -- *** Property
    textMaxLine_
  , includeFontPadding_
  , tailColorConvert_
  , textSingleLineVerticalAlign_
  , textSelection_
  ) where
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
import           Miso.Types (Attribute)
import           Miso.Property
-----------------------------------------------------------------------------
-- | 'textMaxLine_'
--
-- // DefaultValue: '-1'
-- text-maxline?: number;
--
-- Limits the maximum number of lines displayed for the text content,
-- overflow:hidden should be set simultaneously.
--
textMaxLine_ :: Int -> Attribute action
textMaxLine_ = intProp "text-max-line"
-----------------------------------------------------------------------------
-- | 'includeFontPadding_'
--
-- <https://lynxjs.org/api/elements/built-in/text.html#include-font-padding>
--
-- // DefaultValue: false
-- include-font-padding?: boolean;
--
-- Add additional padding for Android text on top and bottom. Enabling this
-- may cause inconsistencies between platforms.
--
includeFontPadding_ :: Bool -> Attribute action
includeFontPadding_ = boolProp "include-font-padding"
-----------------------------------------------------------------------------
-- | 'tailColorConvert_'
--
-- <https://lynxjs.org/api/elements/built-in/text.html#tail-color-convert>
--
-- // DefaultValue: false
-- tail-color-convert?: boolean;
--
-- By default, if the text is truncated, the inserted ... will be displayed with
-- the color specified by the closest inline-text's style. If this attribute
-- is enabled, the color of ... will be specified by the outermost text tag's style.
--
tailColorConvert_ :: Bool -> Attribute action
tailColorConvert_ = boolProp "tail-color-convert"
-----------------------------------------------------------------------------
-- | 'textSingleLineVerticalAlign_'
--
-- <https://lynxjs.org/api/elements/built-in/text.html#text-single-line-vertical-align>
--
-- // DefaultValue: 'normal'
-- text-single-line-vertical-align?: 'normal' | 'top' | 'center' | 'bottom';
--
-- Used to set vertical alignment for single-line plain text. It can be changed
-- by setting 'top' | 'center' | 'bottom'. It is recommended to use this only
-- when the default font does not meet the center alignment requirements, as it
-- increases text measurement time.
--
textSingleLineVerticalAlign_ :: MisoString -> Attribute action
textSingleLineVerticalAlign_ = textProp "text-single-line-vertical-align"
-----------------------------------------------------------------------------
-- | 'textSelection_'
--
-- <https://lynxjs.org/api/elements/built-in/text.html#text-selection>
--
-- // DefaultValue: false
-- text-selection?: boolean;
--
-- Sets whether to enable text selection.
-- When enabled, flatten = False should be set simultaneously.
--
textSelection_ :: Bool -> Attribute action
textSelection_ = boolProp "text-selection"
-----------------------------------------------------------------------------
