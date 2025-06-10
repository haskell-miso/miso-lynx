-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native.Element.Text.Event
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
----------------------------------------------------------------------------
module Miso.Native.Element.Text.Event
  ( -- *** Events
    onBindLayout
    -- *** Types
  , LineInfo (..)
  , Size     (..)
  ) where
-----------------------------------------------------------------------------
import           Data.Aeson
import           Data.Aeson.Types
import           Miso.Event
-----------------------------------------------------------------------------
import           Miso.Types (Attribute)
-----------------------------------------------------------------------------
-- *bindLayout*
--
-- <https://lynxjs.org/api/elements/built-in/image.html#bindlayout>
--
onBindLayout :: (LineInfo -> action) -> Attribute action
onBindLayout action = on "bindlayout" lineInfoDecoder (\e _ -> action e)
-----------------------------------------------------------------------------
data LineInfo
  = LineInfo
  { lineInfoStart :: Int
  , lineInfoEnd :: Int
  , lineInfoEllipsisCount :: Int
  , lineInfoLineCount :: Int
  , lineInfoLines :: [LineInfo]
  , lineInfoSize :: Size
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
instance FromJSON LineInfo where
  parseJSON = lineInfo
-----------------------------------------------------------------------------
data Size
  = Size
  { sizeWidth :: Int
  , sizeHeight :: Int
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
lineInfoDecoder :: Decoder LineInfo
lineInfoDecoder = [] `at` lineInfo
-----------------------------------------------------------------------------
lineInfo :: Value -> Parser LineInfo
lineInfo = withObject "lineInfo" $ \o ->
  LineInfo
    <$> o .: "start"
    <*> o .: "end"
    <*> o .: "ellipsisCount"
    <*> o .: "lineCount"
    <*> o .: "lineInfo"
    <*> do
      s <- o .: "size"
      Size <$> s .: "width" <*> s .: "height"
-----------------------------------------------------------------------------
