module Simulator (simulate,test_Simulator) where

import Test.HUnit
import GameState
import Move
import Card
import Value
import Data.Array
import MoveStep
import Control.Monad.Error

import CardBehavior

simulate :: GameState -> Move -> GameState
simulate gameState move = fst $ runMove moveStep gameState
    where moveStep = do (leftArg, rightArg) <- takeMove move
                        newValue' <- catchError (apply leftArg rightArg) (\e -> return (ValueCard IdentityCard))
                        storeResult newValue' move

takeMove :: Move -> MoveStep (Value, Value)
takeMove (Move applicationDirection card slotNumber)
    = do oldValue <- getProponentField slotNumber -- TODO: error handling needed if slot number bad???
         case applicationDirection of
           LeftApplication -> return ((ValueCard card), oldValue)
           RightApplication -> return (oldValue, (ValueCard card))

storeResult :: Value -> Move -> MoveStep ()
storeResult v (Move _ _ slot) = transformProponentSlots (updateField v slot)

test_Simulator = [
  simulate initialState trivialMove ~?= initialState,
  simulate initialState moveWithResult ~?= alterFirstBoard (updateField (ValueCard ZeroCard) 3) initialState,

  runMove (storeResult (ValueNum 0) (Move undefined undefined 3)) initialState ~?= (alterFirstBoard (updateField (ValueNum 0) 3) initialState, Right ()),

  (runMove (takeMove $ Move LeftApplication ZeroCard 1) initialState
       ~?= (initialState, Right ((ValueCard ZeroCard), (ValueCard IdentityCard)))),
  (runMove (takeMove $ Move RightApplication ZeroCard 1) initialState
       ~?= (initialState, Right ((ValueCard IdentityCard), (ValueCard ZeroCard))))
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
