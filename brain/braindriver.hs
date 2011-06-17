module Main where

import Strategy
import Value
import Card

main :: IO ()
main = do let value = (ValueApplication (ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueCard KCard) (ValueCard IncCard))) (ValueApplication (ValueCard KCard) (ValueCard ZeroCard))) (ValueCard IdentityCard))
              result = buildValue [1..255] 0 value
          print result
