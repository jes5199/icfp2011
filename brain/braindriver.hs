module Main where

import Move
import GameState
import Simulator
import PlayerModel
import System(getArgs)

main :: IO ()
main = do 
    [arg] <- getArgs
    let our_brain   = PlayerModel [gaMakeThisAt arg 0] []
    let their_brain = PlayerModel [gaExternal]  []
    play initialState   [] our_brain   [] their_brain

--
-- Tail recurse through the turns executing the plans devised by the
--     player's brains, asking the brains to plan something else whenever
--     all the moves in the present plan have been exhausted.
--
play :: GameState ->   [Move] -> PlayerModel ->   [Move] -> PlayerModel -> IO ()
play state   [] brain   other_plan other_brain = do
    play state (planSteps (chooseGoal brain state ) state) brain other_plan other_brain
play state   (move:rest_of_plan) brain   other_plan other_brain = do 
    putStr (printMoves [move])
    let (state', err) = simulateTurn (fst $ simulateZombies state) move
    -- switch players and recurse with the player's roles reversed
    let new_state = switchPlayer state'
    play new_state   other_plan other_brain   rest_of_plan brain



