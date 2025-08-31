-----------------------------------------------------------------------------
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Miso hiding (text_)
import           Miso.Lynx
import           Miso.Lynx.Element.View.Event (onTap)
-----------------------------------------------------------------------------
import           Miso.Lens
import           Miso.String
import qualified Miso.CSS as CSS
-----------------------------------------------------------------------------
-- | Application model
newtype Model = Model { _value :: Int }
  deriving (Show, Eq, ToMisoString)
-----------------------------------------------------------------------------
value :: Lens Model Int
value = lens _value $ \m v -> m { _value = v }
-----------------------------------------------------------------------------
data Action
  = AddOne
  | SubtractOne
  deriving (Show, Eq)
-----------------------------------------------------------------------------
-- | Entry point for a miso application
main :: IO ()
main = run $ lynx counterComponent
  { events = lynxEvents
  }
-----------------------------------------------------------------------------
counterComponent :: App Model Action
counterComponent = component (Model 0) updateModel viewModel
-----------------------------------------------------------------------------
updateModel
  :: Action
  -> Transition Model Action
updateModel = \case
  AddOne ->
    value += 1
  SubtractOne ->
    value -= 1
-----------------------------------------------------------------------------
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Model Action
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
