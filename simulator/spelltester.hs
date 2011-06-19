import Test.HUnit
import Spells

tests = test [ --"Spells" ~: test_Strategy
    ]

main = runTestTT tests
