module Main where

import Control.Monad.State
import Strategy
import Value
import Card
import Move
import Parser
import System(getArgs)

main :: IO ()
main = do [arg] <- getArgs
          let value = parse arg
              moves = fst $ runState (buildValue 0 (translateValue value)) [1..255]
          putStr (printMoves (concat [[move, Move LeftApplication IdentityCard 0] | move <- moves]))
