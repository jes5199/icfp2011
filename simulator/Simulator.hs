module Simulator (simulate,test_Simulator) where

import Test.HUnit
import GameState
import Move
import Card
import Value
import Data.Array

simulate gameState who move = gameState

apply :: GameState -> Who -> Move -> Value
apply = undefined

reduce :: Value -> (GameState,Maybe Value)
reduce = undefined

storeResult :: (GameState, Maybe Value) -> Move -> GameState
storeResult = undefined

test_Simulator = [
  simulate initialState FirstPlayer trivialMove ~?= initialState --,
--  simulate initialState FirstPlayer moveWithResult ~?= resultOfMove
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
    resultOfMove = GameState (updateField (ValueNum 0) 3 (firstPlayerBoard initialState)) (secondPlayerBoard initialState)
