import Test.HUnit
import Simulator
import Card
import Value
import CardBehavior

tests = test [ "Simulator" ~: test_Simulator,
               "Card" ~: test_Card,
               "Value" ~: test_Value,
               "CardBehavior" ~: test_CardBehavior ]

main = runTestTT tests
