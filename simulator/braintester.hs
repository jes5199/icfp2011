import Test.HUnit
import SimpleBuilder
import Translator

tests = test [ "SimpleBuilder" ~: test_SimpleBuilder
             , "Translator" ~: test_Translator
             ]

main = runTestTT tests
