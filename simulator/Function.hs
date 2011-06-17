module Function (Function(..),cardToFunction,test_Function) where

import Test.HUnit
import Card

data Function = FunctionCard Card |
                FunctionValue Int |
                FunctionApplication Function Function
              deriving (Eq, Show)

cardToFunction :: Card -> Function
cardToFunction = FunctionCard

test_Function = [
  ] :: [Test]
