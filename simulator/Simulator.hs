module Simulator (simulate,test_Simulator) where

import Test.HUnit
import GameState
import Move
import Card
import Value
import Data.Array
import MoveStep
import Control.Monad.Error

simulate gameState move = gameState

apply :: Move -> MoveStep Value
apply = undefined

reduce :: Value -> MoveStep Value
reduce = undefined

storeResult :: Value -> Move -> MoveStep ()
storeResult v (Move _ _ slot) = transformProponentSlots (updateField v slot)

test_Simulator = [
  simulate initialState trivialMove ~?= initialState,
  runMove (storeResult (ValueNum 0) (Move undefined undefined 3)) initialState ~?= (resultOfMove, Right ())
--  simulate initialState FirstPlayer moveWithResult ~?= resultOfMove
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
    resultOfMove = GameState FirstPlayer
                             (updateField (ValueNum 0) 3 (firstPlayerBoard initialState))
                             (secondPlayerBoard initialState)
