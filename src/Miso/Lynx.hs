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
import Miso (renderComponent, Component)
import Miso.Lynx.Element
import Miso.Lynx.FFI
import Miso.Lynx.Event
-----------------------------------------------------------------------------
import Control.Monad (void)
import Language.Javascript.JSaddle (JSM)
#ifndef GHCJS_BOTH
import Data.FileEmbed (embedStringFile)
import Language.Javascript.JSaddle (eval)
import Miso.String (MisoString)
#endif
-----------------------------------------------------------------------------
lynx :: Eq model => Component model action -> JSM ()
lynx vcomp = withJS $ renderComponent (Just "native") vcomp (pure ())
-----------------------------------------------------------------------------
-- | Used when compiling with jsaddle to make miso's JavaScript present in
-- the execution context.
withJS :: JSM a -> JSM ()
withJS action = void $ do
#ifndef GHCJS_BOTH
  _ <- eval ($(embedStringFile "js/miso-lynx.js") :: MisoString)
#endif
  action
-----------------------------------------------------------------------------
