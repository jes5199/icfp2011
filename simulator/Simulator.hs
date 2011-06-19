module Simulator (simulate,simulateTurn,simulateZombies,test_Simulator) where

import Data.Array
import Control.Monad.Error
import Test.HUnit
import GameState
import Move
import Card
import Value
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
                           (ValueCard IdentityCard)
                           (slotNumOfMove move)
                         throwError e )
  storeResult newValue' (slotNumOfMove move)

storeResult :: Value -> SlotNumber -> MoveStep ()
storeResult v slot = transformProponentSlots (updateField v slot)

runZombiePhase :: GameState -> (GameState, [String])
runZombiePhase state = (\(x,y) -> (x,reverse y))
                       (foldl (\(s,l) n ->
                                let (s',r) = runZombieSlot s n
                                in (s',r ++ l)) (state,[]) [0..255])

lefts :: [Either a b] -> [a]
lefts [] = []
lefts (Left x:xs) = (x : lefts xs)
lefts (Right _:xs) = lefts xs

fromRight :: Either a b -> b
fromRight (Right x) = x

runZombieSlot :: GameState -> SlotNumber -> (GameState,[String])
runZombieSlot state slot =
  case runMove (getProponentVitality slot) state of
    (_,Right (-1)) ->
      let (postZombieState,message) =
            runMove (thisMove $ Move RightApplication IdentityCard slot) state
          (finalState,_) = runMove (putProponentVitality 0 slot >>
                                    putProponentField valueI slot)
                           postZombieState
          applyMsg = "applying zombie slot " ++ show slot ++ "={-1," ++ show (fromRight $ snd $ runMove (getProponentField slot) state) ++ "} to I"
      in case message of
        Left x -> (finalState,[x,applyMsg])
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
