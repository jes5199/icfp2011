module Simulator (simulate,simulateTurn,simulateZombies,test_Simulator) where

import Data.Array
import Control.Monad.Error
import Test.HUnit
import GameState
import Move
import Card
import Value
import Slots
import MoveStep
import CardBehavior
import Data.List (foldl')

simulate :: GameState -> Move -> GameState
simulate currentState move = doneState
  where (zombiedState, _) = simulateZombies currentState
        (playedState, _)  = simulateTurn zombiedState move
        doneState = switchPlayer playedState

simulateTurn :: GameState -> Move -> (GameState, Either String ())
simulateTurn gameState move = runMove (thisMove move) gameState

simulateZombies :: GameState -> (GameState, [String])
simulateZombies state = (finalState, zombieResults)
  where startZombieState = beginZombieApocolypse state
        (endZombieState, zombieResults) = runZombiePhase startZombieState
        finalState = quellZombieApocolypse endZombieState

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
                              state <- getGameState
                              if v <= 0 && not (zombiesAreOut state)
                                then throwError "Dead slot application"
                                else apply leftArg rightArg)
               (\e -> do storeResult
                           valueI
                           (slotNumOfMove move)
                         throwError e )
  storeResult newValue' (slotNumOfMove move)

storeResult :: Value -> SlotNumber -> MoveStep ()
storeResult v slot = transformProponentSlots (updateField v slot)

runZombiePhase :: GameState -> (GameState, [String])
runZombiePhase state =
  (\(x,y) -> (x,reverse y))
  (foldl (\(beforeThisZombie,previousErrors) zombieSlot ->
           let (afterThisZombie,newErrors) = runZombieSlot beforeThisZombie zombieSlot
           in (afterThisZombie,newErrors ++ previousErrors)) (state,[]) [0..255])

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left x:xs) = (x : lefts xs)
lefts (Right _:xs) = lefts xs

fromRight :: Either a b -> b
fromRight (Right x) = x

runZombieSlot :: GameState -> SlotNumber -> (GameState,[String])
runZombieSlot state slot =
  let p = gsMyFriend state in
  case gsGetVitality p state slot of
    (-1) ->
      let zombieMove = Move RightApplication IdentityCard slot
          (postZombieState,result) = runMove (thisMove zombieMove) state
          finalState = gsSetField              slot valueI p $
                       gsSetVitalityOnDeadSlot slot 0      p $
                       postZombieState
          applyMsg = "applying zombie slot " ++ show slot ++ "={-1," ++
                     show (gsGetField p state slot) ++ "} to I"
      in case result of
        Left x -> (finalState, ["Exception: "++ x, applyMsg])
        _ -> (finalState,[applyMsg])
    _ -> (state, [])

test_Simulator = [
  simulateTurn initialState trivialMove ~?= (initialState, Right ()),
  simulateTurn initialState moveWithResult ~?= (alterFirstBoard (updateField valueZero 3) initialState, Right ()),

  runMove (storeResult (ValueNum 0) 3) initialState ~?= (alterFirstBoard (updateField (ValueNum 0) 3) initialState, Right ()),

  runMove (fetchValues $ Move LeftApplication ZeroCard 1) initialState
    ~?= (initialState, Right (valueZero, valueI)),
  runMove (fetchValues $ Move RightApplication ZeroCard 1) initialState
    ~?= (initialState, Right (valueI, valueZero))
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
    moveWithResult = Move RightApplication ZeroCard 3
