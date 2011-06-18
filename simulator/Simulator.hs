module Simulator (simulate,test_Simulator) where

import Data.Array
import Control.Monad.Error
import Test.HUnit
import GameState
import Move
import Card
import Value
import MoveStep
import CardBehavior

simulate :: GameState -> Move -> (GameState, Either String ())
simulate gameState move = runMove moveStep gameState
    where moveStep = do (leftArg, rightArg) <- takeMove move
                        newValue' <- catchError (apply leftArg rightArg)
                            (\e -> do storeResult (ValueCard IdentityCard) move
                                      throwError e )
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
  simulate initialState trivialMove ~?= (initialState, Right ()),
  simulate initialState moveWithResult ~?= (alterFirstBoard (updateField valueZero 3) initialState, Right ()),

  runMove (storeResult (ValueNum 0) (Move undefined undefined 3)) initialState ~?= (alterFirstBoard (updateField (ValueNum 0) 3) initialState, Right ()),

  (runMove (takeMove $ Move LeftApplication ZeroCard 1) initialState
       ~?= (initialState, Right (valueZero, (valueI)))),
  (runMove (takeMove $ Move RightApplication ZeroCard 1) initialState
       ~?= (initialState, Right (valueI, valueZero)))
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
