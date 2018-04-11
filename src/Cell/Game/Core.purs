module Cell.Game.Core where

import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import LinearAlgebra.Matrix (fromColumn, element)
import Data.Coyoneda (Coyoneda, unCoyoneda, liftCoyoneda)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.HalogenM as HQ

import Cell.Board.Component as B
import Depo.Type (Board, class CanSet, set, Status(..), Game, Slot, Message(..))


data Mode f i o a = 
    Play a
    | Set a
    -- | Contains a query of the inner component
    | Inner (Coyoneda f a)
    -- | React to input to the HOC
    | InnerInput i a
    -- | Handle messages of te inner component
    | HandleInner o a


liftQuery :: forall f i o a. f a -> Mode f i o a
liftQuery = Inner <<< liftCoyoneda

game 
    :: forall f i o m 
    . CanSet f 
    => H.Component HH.HTML f i o m 
    -> H.Component HH.HTML (Mode f i o) i o m
game innerComponent =
    H.parentComponent 
        { initialState: { on: false, input: _ }
        , render 
        , eval
        , receiver: \i -> Just $ InnerInput i unit
        }
    where

    render :: Game i -> H.ParentHTML (Mode f i o) f Slot m
    render state =
            HH.div_
              [ HH.hr_
              , HH.p_
                [ HH.button
                  [ HE.onClick (HE.input_ Set) ]
                  [ HH.text "play" ]
                , HH.text $ " Wrapper state: " <> if state.on then "on" else "off"
                ]
              , HH.p_
                [ HH.button
                  [ HE.onClick (HE.input_ Play) ]
                  [ HH.text "begin" ]
                ]
              , HH.p_
                [ HH.slot unit innerComponent state.input (HE.input HandleInner)
                ]
              , HH.hr_
              ]

    eval :: Mode f i o ~> H.ParentDSL (Game i) (Mode f i o) f Slot o m
    eval (Play next) = do
        H.modify $ \state -> state { on = not state.on }
        pure next
    eval (Set next) = do
        _ <- H.query unit $ H.action (set false)
        pure next
    eval (Inner iq) = 
        iq # unCoyoneda \k q -> do
            result <- H.query unit q
            case result of
                Nothing ->
                    HQ.halt "HOC inner component query failed (this should be impossible)"
                Just a -> pure (k a)
    eval (HandleInner o next) = do
        H.raise o
        pure next
    eval (InnerInput i next) = do
        H.modify $ _{ input = i }
        pure next

    -- eval =
    --     case _ of
    --         Begin next -> do
    --             {board, status} <- H.get 
    --             let curState = fromMaybe false $ element 0 0 board
    --                 nextState = if curState then Start else End
    --                 nextGame = { board: board, status: nextState }
    --             H.put $ nextGame
    --             H.raise $ Playing nextGame

    --             pure next 
    --         Turn reply -> do 
    --             state <- H.get
    --             pure (reply state)
