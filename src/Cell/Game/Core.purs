module Game.Core where

import Prelude
import Data.Maybe (Maybe(..), maybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Board.Square as Squar
import Game.UI as UI

data Query a
  = HandleSquare Squar.Output a
  | CheckSquareState a

type State =
  { toggleCount :: Int
  , buttonState :: Maybe Boolean
  }

data Slot = Square
derive instance eqTickSlot :: Eq Slot
derive instance ordTickSlot :: Ord Slot

type Frame = UI.Query Squar.Input Squar.State Squar.Output

game :: forall m. H.Component HH.HTML Query Unit Void m
game =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { toggleCount: 0
    , buttonState: Nothing
    }

  render :: State -> H.ParentHTML Query Frame Slot m
  render state =
    HH.div_
      [ HH.slot Square (UI.factory Squar.mySquare) true (HE.input HandleSquare)
      , HH.p_
          [ HH.text ("Square has been toggled " <> show state.toggleCount <> " time(s)") ]
      , HH.p_
          [ HH.text
              $ "Last time I checked, the button was: "
              <> (maybe "(not checked yet)" (if _ then "on" else "off") state.buttonState)
              <> ". "
          , HH.button
              [ HE.onClick $ HE.input_ CheckSquareState ]
              [ HH.text "Check now" ]
          ]
      ]

  eval :: Query ~> H.ParentDSL State Query Frame Slot Void m
  eval = case _ of
    HandleSquare (Squar.Toggled _) next -> do
      H.modify (\st -> st { toggleCount = st.toggleCount + 1 })
      pure next
    CheckSquareState next -> do
      buttonState <- H.query Square $ UI.liftQuery $ H.request Squar.IsOn
      H.modify (_ { buttonState = buttonState })
      pure next
