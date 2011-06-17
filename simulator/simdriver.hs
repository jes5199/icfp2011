module Main where

import Card
import Move
import GameState
import Simulator

main :: IO ()
main = do let state = initialState
              move = Move LeftApplication IdentityCard 1
              result = simulate state move
          print result
