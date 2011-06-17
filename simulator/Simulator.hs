module Simulator (simulate,test_Simulator) where

import Test.HUnit

simulate x = id

test_Simulator = [
  simulate "initial state" "some move" ~?= "some move"
  ]
