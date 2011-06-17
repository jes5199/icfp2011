module Simulator (simulate,test_Simulator) where

import Test.HUnit
import GameState
import Move
import Card

simulate x y = x

test_Simulator = [
  simulate initialState trivialMove ~?= initialState
  ]
  where
    trivialMove = Move FirstPlayer LeftApplication IdentityCard 2
