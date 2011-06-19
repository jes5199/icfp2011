import Test.HUnit
import Strategy
import Translator

tests = test [ "Strategy" ~: test_Strategy
             , "Translator" ~: test_Translator
             ]

main = runTestTT tests
