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
  whatToApply <- fetchMove move
  case whatToApply of
    Nothing -> return ()
    Just (leftArg, rightArg) -> tryApply move leftArg rightArg

fetchMove :: Move -> MoveStep (Maybe (Value, Value))
fetchMove (Move applicationDirection card slotNumber) = do
  v <- getProponentVitality slotNumber
  if v <= 0
    then return Nothing
    else do oldValue <- getProponentField slotNumber
            return $ case applicationDirection of
              LeftApplication -> Just ((ValueCard card), oldValue)
              RightApplication -> Just (oldValue, (ValueCard card))

tryApply :: Move -> Value -> Value -> MoveStep ()
tryApply move leftArg rightArg = do
  newValue' <- catchError (apply leftArg rightArg)
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

  runMove (fetchMove $ Move LeftApplication ZeroCard 1) initialState
    ~?= (initialState, Right (Just (valueZero, valueI))),
  runMove (fetchMove $ Move RightApplication ZeroCard 1) initialState
    ~?= (initialState, Right (Just (valueI, valueZero)))
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
