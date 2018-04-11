module Cell.Game.UI where

import Prelude
import Data.Maybe (Maybe(..), maybe)

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Depo.Type (UIQuery(..), Query(..), UIState(..), SlotBoard, Message(..))
import Cell.Board.Component as Board
import Cell.Game.Core (game, Mode(..), liftQuery)

type GameMode = Mode Query Boolean Message

ui :: forall m. H.Component HH.HTML UIQuery Unit Void m
ui =
    H.parentComponent
        { initialState: const initialState
        , render
        , eval
        , receiver: const Nothing
        }
    where

    initialState :: UIState
    initialState =
        { toggleCount: 0
        , boardState: Nothing
        }

    render :: UIState -> H.ParentHTML UIQuery GameMode SlotBoard m
    render state =
        HH.div_
        [ HH.slot SlotBoard (game Board.board) true (HE.input HandleBoard)
        , HH.p_
            [ HH.text ("Button has been toggled " <> show state.toggleCount <> " time(s)") ]
        , HH.p_
            [ HH.text
                $ "Last time I checked, the button was: "
                <> (maybe "(not checked yet)" (if _ then "on" else "off") state.boardState)
                <> ". "
            , HH.button
                [ HE.onClick (HE.input_ CheckBoardState) ]
                [ HH.text "Check now" ]
            ]
        ]

    eval :: UIQuery ~> H.ParentDSL UIState UIQuery GameMode SlotBoard Void m
    eval = case _ of
        HandleBoard (Toggled _) next -> do
            H.modify (\st -> st { toggleCount = st.toggleCount + 1 })
            pure next
        CheckBoardState next -> do
            boardState <- H.query Board $ liftQuery $ H.request IsOn
            H.modify (_ { boardState = boardState })
            pure next    