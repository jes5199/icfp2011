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
    let our_moves = planSteps (chooseGoal arg)
    let their_moves = take (length our_moves) ((Move LeftApplication IdentityCard 0) : their_moves)
    putStr (printMoves (concat [[our_move, their_move] | our_move <- our_moves, their_move <- their_moves]))

chooseGoal arg = do
    let value = parse arg
    translateValue value

planSteps goal = fst $ runState (buildValue 0 goal) [1..255]
