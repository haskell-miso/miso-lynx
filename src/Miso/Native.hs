-----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
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
     -- * Event
   , module Miso.Native.Event
   ) where
-----------------------------------------------------------------------------
import Miso (renderComponent, Component)
import Miso.Native.Element
import Miso.Native.FFI
import Miso.Native.Event
-----------------------------------------------------------------------------
import Control.Monad (void)
import Language.Javascript.JSaddle (JSM)
#ifndef GHCJS_BOTH
import Data.FileEmbed (embedStringFile)
import Language.Javascript.JSaddle (eval)
import Miso.String (MisoString)
#endif
-----------------------------------------------------------------------------
native :: Eq model => Component name model action -> JSM ()
native vcomp = withJS $ renderComponent (Just "native") vcomp (pure ())
-----------------------------------------------------------------------------
-- | Used when compiling with jsaddle to make miso's JavaScript present in
-- the execution context.
withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef GHCJS_BOTH
  _ <- eval ($(embedStringFile "js/miso-native.js") :: MisoString)
#endif
  action
-----------------------------------------------------------------------------
