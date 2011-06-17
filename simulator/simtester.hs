import Simulator

import Test.HUnit

tests = test [ "Simulator" ~: test_Simulator ]

main = runTestTT tests
