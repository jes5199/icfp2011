module Main where

import Move
import Card
import GameState
import Simulator
import PlayerModel
import System(getArgs)
import System.IO (hFlush,stdout)
--import Planner
import AltPlanner

main :: IO ()
main = do
    [arg] <- getArgs
    let our_brain   = altPlanner -- makePlanner [] [] -- ModeledPlayer [(gaMakeThisAt "6" 0),gaIForever] []
    let their_brain = ExternalPlayer
    if (arg == "0")
       then play initialState [] our_brain   [] their_brain
       else play initialState [] their_brain [] our_brain

--
-- Tail recurse through the turns executing the plans devised by the
--     player's brains, asking the brains to plan something else whenever
--     all the moves in the present plan have been exhausted.
--
play :: GameState ->   [Move] -> PlayerModel ->   [Move] -> PlayerModel -> IO ()
play state   [] brain   other_plan other_brain = do
    new_plan <- case brain of
        (ModeledPlayer _ _) -> return (planSteps (chooseGoal brain state ) state)
        (PurePlayer f) -> return (f state)
        (ExternalPlayer) -> do
            applicationDir <- readInt
            move <- case applicationDir of
              1 -> do
                  card <- getLine
                  slotNumber <- readInt
                  return $ Move LeftApplication (readCard card) slotNumber
              2 -> do
                  slotNumber <- readInt
                  card <- getLine
                  return $ Move RightApplication (readCard card) slotNumber
            return [move]
    play state new_plan brain other_plan other_brain

play state   (move:rest_of_plan) brain   other_plan other_brain = do
    case brain of
        (ExternalPlayer) -> do return ()
        _ -> do mapM_ putStrLn (printMove move)
    hFlush stdout
    let new_state = simulate state move
    play new_state   other_plan other_brain   rest_of_plan brain

readInt :: IO Int
readInt = do
    str <- getLine
    return $ read str

