module Main where

import Card
import Move
import Simulator

main :: IO ()
main = do let state = ()
              move = Move LeftApplication (Card "fake") 1
              result = simulate state move
          print result
