import Test.HUnit
import Simulator
import Card
import Function

tests = test [ "Simulator" ~: test_Simulator,
               "Card" ~: test_Card,
               "Function" ~: test_Function ]

main = runTestTT tests
