-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Miso.Native
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
-- import Miso.Native
--
-- view :: Model -> View Action
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
-- <http://github.com/dmjio/miso-native>
--
----------------------------------------------------------------------------
module Miso.Native
   ( -- * Entrypoint
     native
     -- * Element
   , module Miso.Native.Element
     -- * FFI
   , module Miso.Native.FFI
   ) where
-----------------------------------------------------------------------------
import Miso (renderComponent, Component)
import Miso.Native.Element
import Miso.Native.FFI
-----------------------------------------------------------------------------
import Language.Javascript.JSaddle
-----------------------------------------------------------------------------
native :: Eq model => Component name model action -> JSM ()
native vcomp = renderComponent (Just "native") vcomp (pure ())
-----------------------------------------------------------------------------
