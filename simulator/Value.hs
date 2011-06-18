module Value (Value(..),cardToValue,test_Value) where

import Test.HUnit
import Card

data Value = ValueCard Card
           | ValueNum Int
           | ValueApplication Value Value
           | ValueLambda String Value
           | ValueVariable String
           deriving (Eq)

instance Show Value where
  show (ValueCard c) = show c
  show (ValueNum n) = show n
  show (ValueApplication x y) = show x ++ "(" ++ show y ++ ")"
  show (ValueLambda s v) = "λ" ++ s ++ "." ++ show v
  show (ValueVariable s) = s

cardToValue :: Card -> Value
cardToValue = ValueCard

test_Value = [
  ] :: [Test]
