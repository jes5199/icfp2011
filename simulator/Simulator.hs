module Simulator (simulate,test_Simulator) where

import Test.HUnit
import GameState
import Move
import Card

simulate x who y = x

test_Simulator = [
  simulate initialState FirstPlayer trivialMove ~?= initialState
  ]
  where
    trivialMove = Move LeftApplication IdentityCard 2
