module Simulator (simulate,test_Simulator) where

import Test.HUnit

simulate x y = (x,y)

test_Simulator = [
  simulate "initial state" "some move" ~?= ("initial state","some move")
  ]
