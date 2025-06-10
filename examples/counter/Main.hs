{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Haskell module declaration
module Main where

-- | Miso framework import
import           Prelude hiding (unlines)

import           Miso hiding (id_)
import           Miso.Lens
import           Miso.String
import           Miso.Native
import qualified Miso.Style as CSS
import qualified Data.Map.Strict as M

-- | Type synonym for an application model
newtype Model = Model { _value :: Int }
  deriving stock (Show, Eq)

instance ToMisoString Model where
  toMisoString (Model v) = toMisoString v

value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }

-- | Sum type for application events
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  | Tap Int
  deriving (Show, Eq)

-- | Entry point for a miso application
main :: IO ()
main = run $ native vcomp
  { events = M.singleton "tap" False
  , initialAction = Just SayHelloWorld
  }

-- | Componentlication definition (uses 'defaultComponent' smart constructor)
vcomp :: Component "counter" Model Action
vcomp = defaultComponent (Model 0) updateModel viewModel

-- | UpdateModels model, optionally introduces side effects
updateModel
  :: Action
  -> Effect Model Action
updateModel SayHelloWorld =
  io_ (consoleLog "Inside Say Hello World!")
updateModel AddOne = do
  io_ (consoleLog "Inside AddOne")
  value += 1
updateModel SubtractOne = do
  io_ (consoleLog "Inside SubtractOne")
  value -= 1
updateModel (Tap x) =
  io_ $ consoleLog ("tapped: " <> ms (show x))

-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = view_ []
  [ view_
    [ onTap AddOne
--    , id_ "plus"
    , CSS.style_
        [ CSS.backgroundColor CSS.yellow
        , CSS.width "100px"
        , CSS.height "100px"
        ]
    ]
    [ text_ [  ] [ "+" ]
    ]
  , view_
    [ CSS.style_
        [ CSS.backgroundColor CSS.orange
        , CSS.width "100px"
        , CSS.height "100px"
        ]
    ]
    [ text_
      []
      [ "Miso counter app!"
      , text ("value: " <> ms m)
      ]
    ]
  , view_
    [ onTap SubtractOne
--    , id_ "minus"
    , CSS.style_
        [ CSS.backgroundColor CSS.pink
        , CSS.width "100px"
        , CSS.height "100px"
        ]

    ]
    [ text_ [ ] [ "-" ]
    ]
  ]
