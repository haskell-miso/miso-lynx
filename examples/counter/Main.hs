-----------------------------------------------------------------------------
{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso
import           Miso.Native
import           Miso.Native.Element.View.Event (onTap)
-----------------------------------------------------------------------------
import           Miso.Lens
import           Miso.String
import qualified Miso.Style as CSS
-----------------------------------------------------------------------------
-- | Type synonym for an application model
newtype Model = Model { _value :: Int }
  deriving (Show, Eq, ToMisoString)
-----------------------------------------------------------------------------
value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
data Action
  = AddOne
  | SubtractOne
  | SayHelloWorld
  | Tap Int
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run $ native counterComponent
  { events = nativeEvents
  , initialAction = Just SayHelloWorld
  }
-----------------------------------------------------------------------------
counterComponent :: Component Model Action
counterComponent = component (Model 0) updateModel viewModel
-----------------------------------------------------------------------------
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
-----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m = view_
  [ CSS.style_
    [ CSS.height "200px"
    , CSS.display "flex"
    , CSS.alignItems "center"
    , CSS.justifyContent "center"
    ]
  ]
  [ view_
    [ onTap AddOne
    , id_ "plus"
    , CSS.style_
        [ CSS.backgroundColor CSS.yellow
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.margin "2px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ "🐈"
      ]
    ]
  , view_
    [ CSS.style_
        [ CSS.backgroundColor CSS.orange
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ text $ ms (m ^. value)
      ]
    ]
  , view_
    [ onTap SubtractOne
    , id_ "minus"
    , CSS.style_
        [ CSS.backgroundColor CSS.pink
        , CSS.width "100px"
        , CSS.height "100px"
        , CSS.margin "2px"
        , CSS.display "flex"
        , CSS.alignItems "center"
        , CSS.justifyContent "center"
        ]
    ]
    [ text_
      [ CSS.style_
        [ CSS.fontSize "48px"
        ]
      ]
      [ "🍜"
      ]
    ]
 ]
-----------------------------------------------------------------------------
