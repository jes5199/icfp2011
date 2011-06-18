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
    let our_moves   = planSteps (chooseGoal arg)
    let their_moves = planSteps (0,ValueCard IdentityCard) ++ their_moves
    putStr (printMoves (concat [[our_move, their_move] | (our_move,their_move) <- zip our_moves their_moves]))

chooseGoal arg = do
    let value = parse arg
    (0, translateValue value)

planSteps goal = fst $ runState (buildValue (fst goal) (snd goal)) [1..255]
