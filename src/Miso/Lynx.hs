-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Lynx
-- Copyright   :  (C) 2016-2025 David M. Johnson
-- License     :  BSD3-style (see the file LICENSE)
-- Maintainer  :  David M. Johnson <code@dmj.io>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Example usage:
--
-- @
-- import Miso
-- import Miso.Lynx
--
-- view :: Model -> View Model Action
-- view m =
--   view_
--   []
--   [ view_ [ onTap Increment ] [ text_ [] [ "+" ] ]
--   , text_ [] [ text $ ms (show m) ]
--   , view_ [ onTap Decrement ] [ text_ [] [ "-" ] ]
--   ]
-- @
--
-- More information on how to use miso is available on GitHub
--
-- <http://github.com/dmjio/miso-lynx>
--
----------------------------------------------------------------------------
module Miso.Lynx
   ( -- * Entrypoint
     lynx
     -- * Element
   , module Miso.Lynx.Element
     -- * FFI
   , module Miso.Lynx.FFI
     -- * Event
   , module Miso.Lynx.Event
   ) where
-----------------------------------------------------------------------------
import Miso (renderApp, App)
import Miso.Lynx.Element
import Miso.Lynx.FFI
import Miso.Lynx.Event
-----------------------------------------------------------------------------
import Control.Monad (void)
-----------------------------------------------------------------------------
lynx :: Eq model => App model action -> IO ()
lynx vcomp = withJS (renderApp "native" vcomp)
-----------------------------------------------------------------------------
-- | Used when compiling with jsaddle to make miso's JavaScript present in
-- the execution context.
withJS :: IO a -> IO ()
withJS action = void $ do
#ifdef WASM
  $(evalFile "js/miso-lynx.js")
#endif
  action
-----------------------------------------------------------------------------
