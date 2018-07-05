module Board.Square where

import Prelude
import Data.Maybe (Maybe(..))

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Game.UI (class CanSet)

type State = Boolean

data Input a
  = Toggle a
  | Set Boolean a
  | IsOn (Boolean -> a)

data Output = Toggled Boolean

instance canSetQuery :: CanSet Input where
  set = Set

-- why boolean instaed of State?
-- mySquare :: forall m. H.Component HH.HTML Input Boolean Output m
mySquare :: forall o. H.Component HH.HTML Input State Output o
mySquare =  
  H.component
    { initialState: id
    , render
    , eval
    , receiver: const Nothing
    }
  where

  render :: State -> H.ComponentHTML Input
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.button
        [ HP.title label
        , HE.onClick (HE.input_ Toggle)
        ]
        [ HH.text label ]

  eval :: Input ~> H.ComponentDSL State Input Output o
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    Set value next -> do
      H.put value
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)
