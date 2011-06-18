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
simulate gameState move = runMove (thisMove move) gameState

thisMove :: Move -> MoveStep ()
thisMove move = do
  (leftArg, rightArg) <- fetchValues move
  tryApply move leftArg rightArg

fetchValues :: Move -> MoveStep (Value, Value)
fetchValues (Move applicationDirection card slotNumber) = do
  oldValue <- getProponentField slotNumber
  return $ case applicationDirection of
    LeftApplication -> (ValueCard card, oldValue)
    RightApplication -> (oldValue, ValueCard card)

tryApply :: Move -> Value -> Value -> MoveStep ()
tryApply move leftArg rightArg = do
  newValue' <- catchError (do v <- getProponentVitality (slotNumOfMove move)
                              if v <= 0
                                then throwError "Dead slot application"
                                else apply leftArg rightArg)
               (\e -> do storeResult
                           (ValueCard IdentityCard)
                           (slotNumOfMove move)
                         throwError e )
  storeResult newValue' (slotNumOfMove move)

storeResult :: Value -> SlotNumber -> MoveStep ()
storeResult v slot = transformProponentSlots (updateField v slot)

test_Simulator = [
  simulate initialState trivialMove ~?= (initialState, Right ()),
  simulate initialState moveWithResult ~?= (alterFirstBoard (updateField valueZero 3) initialState, Right ()),

  runMove (storeResult (ValueNum 0) 3) initialState ~?= (alterFirstBoard (updateField (ValueNum 0) 3) initialState, Right ()),

  runMove (fetchValues $ Move LeftApplication ZeroCard 1) initialState
    ~?= (initialState, Right (valueZero, valueI)),
  runMove (fetchValues $ Move RightApplication ZeroCard 1) initialState
    ~?= (initialState, Right (valueI, valueZero))
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
