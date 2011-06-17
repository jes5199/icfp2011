import Test.HUnit
import Simulator
import Card
import Function
import CardBehavior

tests = test [ "Simulator" ~: test_Simulator,
               "Card" ~: test_Card,
               "Function" ~: test_Function,
               "CardBehavior" ~: test_CardBehavior ]

main = runTestTT tests
