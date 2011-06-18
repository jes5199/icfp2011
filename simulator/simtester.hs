import Test.HUnit
import Simulator
import Card
import Value
import GameState
import MoveStep
import CardBehavior

tests = test [ "Simulator" ~: test_Simulator,
               "Card" ~: test_Card,
               "Value" ~: test_Value,
               "GameState" ~: test_GameState,
               "MoveStep" ~: test_MoveStep,
               "CardBehavior" ~: test_CardBehavior ]

main = runTestTT tests
