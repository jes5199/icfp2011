module Value (Value(..),cardToValue,test_Value) where

import Test.HUnit
import Card

data Value = ValueCard Card
           | ValueNum Int
           | ValueApplication Value Value
           | ValueLambda String Value
           | ValueVariable String
           deriving (Eq, Show)

cardToValue :: Card -> Value
cardToValue = ValueCard

test_Value = [
  ] :: [Test]
