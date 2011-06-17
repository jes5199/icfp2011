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
              result = fst $ runState (buildValue 0 (translateNums value)) [1..255]
          putStr (printMoves result)
