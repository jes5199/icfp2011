module Main where

import Control.Monad.State
import Strategy
import Value
import Card
import Move
import GameState
import Simulator
import Parser
import System(getArgs)

main :: IO ()
main = do 
    [arg] <- getArgs
    let game_state   = initialState
    let our_agents   = [gaMakeThisAt arg 0]
    let their_agents = [gaMakeThisAt "I" 0]
    let our_moves   = planSteps (chooseGoal   our_agents game_state ) game_state
    let their_moves = planSteps (chooseGoal their_agents game_state ) game_state ++ their_moves
    play our_moves their_moves initialState

play :: [Move] -> [Move] -> GameState -> IO ()
play (move:moves) other_moves state = do 
    putStr (printMoves [move])
    new_state <- playOneMove move state
    play other_moves moves new_state

playOneMove :: Move -> GameState -> IO GameState
playOneMove move state = do
     let (state', err) = simulate state move
     return $ switchPlayer state'

--
-- Goal Agents
--

-- Make a specfic thing, ignoring game state
gaMakeThisAt what targetCell game_state = (0, translateValue (parse what))

-- TODO: read and parse the action they took and assume that doing that was their goal
--     (Note: this should be expressed as a "do this", not acomplish this) 
gaExternal game_state = undefined



--
-- Choose best goal based on all sorts of clever logic
--      For now, use "first thing on the list"
--
chooseGoal goal_agents game_state =
    let goals = ($game_state) `map` goal_agents
    in head goals

--
-- Figure out the next steps(s) to accomplish a given goal
--
planSteps = planStepsBlindly

-- Blindly do the whole thing ignoring state
planStepsBlindly goal game_state = fst $ runState (buildValue (fst goal) (snd goal)) [1..255]




--
-- Utilities
--
interleave :: [a] -> [a] -> [a]
interleave as bs = concat [[a, b] | (a,b) <- zip as bs]


