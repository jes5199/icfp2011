import Test.HUnit
import Simulator
import Card
import Value
import GameState
import CardBehavior

tests = test [ "Simulator" ~: test_Simulator,
               "Card" ~: test_Card,
               "Value" ~: test_Value,
               "GameState" ~: test_GameState,
               "CardBehavior" ~: test_CardBehavior ]

main = runTestTT tests
