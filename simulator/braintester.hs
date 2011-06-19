import Test.HUnit
import Strategy

tests = test [ "Strategy" ~: test_Strategy ]

main = runTestTT tests
