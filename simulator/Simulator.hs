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
apply (Move applicationDirection card slotNumber)
    = do oldValue <- getProponentSlotField slotNumber -- TODO: error handling needed if slot number bad???
         case applicationDirection of
           LeftApplication -> return (ValueApplication (ValueCard card) oldValue)
           RightApplication -> return (ValueApplication oldValue (ValueCard card))

reduce :: Value -> MoveStep Value
reduce = undefined

storeResult :: Value -> Move -> MoveStep ()
storeResult v (Move _ _ slot) = transformProponentSlots (updateField v slot)

test_Simulator = [
  simulate initialState trivialMove ~?= initialState,
  runMove (storeResult (ValueNum 0) (Move undefined undefined 3)) initialState ~?= (resultOfMove, Right ()),
  (runMove (apply $ Move LeftApplication ZeroCard 1) initialState
   ~?= (initialState, Right (ValueApplication (ValueCard ZeroCard) (ValueCard IdentityCard))))
--  simulate initialState FirstPlayer moveWithResult ~?= resultOfMove
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
    resultOfMove = GameState FirstPlayer
                             (updateField (ValueNum 0) 3 (firstPlayerBoard initialState))
                             (secondPlayerBoard initialState)
