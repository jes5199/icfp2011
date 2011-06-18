module Main where

import Control.Monad.State
import Strategy
import Value
import Card
import Move
import Parser
import System(getArgs)

main :: IO ()
main = do 
    [arg] <- getArgs
    let game_state = () -- yeah, there's a game state, but we don't know what it is yet
    let our_agents   = [gaMakeThisAt arg 0]
    let their_agents = [gaMakeThisAt "I" 0]
    let our_moves   = planSteps (chooseGoal   our_agents game_state ) game_state
    let their_moves = planSteps (chooseGoal their_agents game_state ) game_state ++ their_moves
    putStr (printMoves (concat [[our_move, their_move] | (our_move,their_move) <- zip our_moves their_moves]))

--
-- Goal Agents
--

-- Make a specfic thing, ignoring game state
gaMakeThisAt what targetCell game_state = (0, translateValue (parse what))

--
-- Choose best goal based on all sorts of clever logic
--      For now, use "first thing on the list"
--
chooseGoal goal_agents game_state = (goal_agents !! 0) game_state

--
-- Figure out the next steps(s) to accomplish a goal
--
planSteps = planStepsBlindly

-- Blindly do the whole thing ignoring state
planStepsBlindly goal game_state = fst $ runState (buildValue (fst goal) (snd goal)) [1..255]
