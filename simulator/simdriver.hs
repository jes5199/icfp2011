module Main where

import Card
import Move
import GameState
import Simulator

main :: IO ()
main = do let state = initialState
              move = Move FirstPlayer LeftApplication (Card "fake") 1
              result = simulate state move
          print result
