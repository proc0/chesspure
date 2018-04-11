module Depo.Type where

import Prelude
import Data.Coyoneda (Coyoneda, liftCoyoneda)
import Data.Maybe (Maybe)
import Halogen as H

-- import Prelude
import LinearAlgebra.Matrix (Matrix)


type Slot = Unit

type Game i = 
    { on :: Boolean
    , input :: i
    }


data Status =
    Start | End

-- data Mode a =
--     Begin a 
--     | Turn (Game -> a)


-- data Loop a = Playing (Game a)

class CanSet f where
  set :: Boolean -> H.Action f

instance canSetQuery :: CanSet Query where
  set = Set

-- data Loop f i o a = 
--     Playing a
--     -- | Contains a query of the inner component
--     | Inner (Coyoneda f a)
--     -- | Handle messages of te inner component
--     | HandleInner o a
--     -- | React to input to the HOC
--     | InnerInput i a

type Board = Matrix Boolean

data SlotBoard = Board
derive instance eqTickSlot :: Eq SlotBoard
derive instance ordTickSlot :: Ord SlotBoard


data Query a = 
    Toggle a 
    | Set Boolean a
    | IsOn (Board -> a)

data Message = Toggled Board

data UIQuery a
  = HandleBoard Message a
  | CheckBoardState a

type UIState =
    { toggleCount :: Int
    , boardState :: Board
    }