module Cell.Board.Component where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import LinearAlgebra.Matrix (fromColumn, element)

import Depo.Type (Board, Query(..), Message(..))



board :: forall m. H.Component HH.HTML Query Unit Message m 
board =
    H.component 
        { initialState: const initialState
        , render 
        , eval
        , receiver: const Nothing
        }
    where

    initialState :: Board
    initialState = fromColumn [false]

    render :: Board -> H.ComponentHTML Query
    render bd =
            let l = fromMaybe false $ element 0 0 bd
                label = if l then "on" else "off"
            in 
                HH.button
                    [ HP.title label
                    , HE.onClick (HE.input_ Toggle)
                    ]
                    [ HH.text label 
                    ]

    eval :: Query ~> H.ComponentDSL Board Query Message m
    eval =
        case _ of
            Toggle next -> do
                state <- H.get 
                let curState = fromMaybe false $ element 0 0 state
                    nextState = fromColumn [not curState]
                H.put $ nextState
                H.raise $ Toggled nextState
                pure next
            Set value next -> do
                H.put $ fromColumn [value]
                pure next
            IsOn reply -> do 
                state <- H.get
                pure (reply state)