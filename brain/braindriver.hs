module Main where

import Control.Monad.State
import Strategy
import Value
import Card
import Move

main :: IO ()
main = do let value = (ValueApplication (ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueCard KCard) (ValueCard IncCard))) (ValueApplication (ValueCard KCard) (ValueCard ZeroCard))) (ValueCard IdentityCard))
              result = fst $ runState (buildValue 0 value) [1..255]
          putStr (printMoves result)
