module Translator(test_Translator, translateValue, translateNum, makeK, template) where

import Test.HUnit ( (~?=) )

import Value
import Card
import Parser

-- Translate the given value until everythin is expressed in terms of
-- ValueApplication, ValueCard, and ValueVariable for unbound
-- variables.
--
-- Modelled after Lisp macro expansion.
translateValue card = case card of
                        (ValueCard c) -> ValueCard c
                        (ValueNum n) -> expandMacro (ValueNum n)
                        (ValueApplication f x) -> expandMacro (ValueApplication (translateValue f) (translateValue x))
                        (ValueLambda x v) -> expandMacro (ValueLambda x (translateValue v))
                        (ValueVariable x) -> ValueVariable x
    where expandMacro (ValueLambda varName (ValueVariable x)) | varName == x = ValueCard IdentityCard
          expandMacro (ValueLambda varName value) | not (value `includes` varName) = makeK value
          expandMacro (ValueLambda varName (ValueApplication f (ValueVariable x)))
              | varName == x && not (f `includes` varName) = f
          expandMacro (ValueLambda varName (ValueApplication f x))
              = ValueApplication (ValueApplication (ValueCard SCard) (expandMacro (ValueLambda varName f)))
                                 (expandMacro (ValueLambda varName x))
          expandMacro (ValueApplication (ValueVariable "lazy") value) = lazify value
          expandMacro value = value
          ValueVariable x `includes` varName = x == varName
          ValueApplication f x `includes` varName = (f `includes` varName) || (x `includes` varName)
          ValueLambda varNameInner value `includes` varName | varNameInner == varName = False
                                                            | otherwise = value `includes` varName
          _ `includes` varName = False
          lazify (ValueApplication f x) = ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueApplication (ValueCard SCard) (makeK f)) (makeK x))) (ValueCard IdentityCard)
          lazify value = value

-- Like translateValue, but translate numbers to sequences of succ, dbl, and zero.
translateNum 0 = ValueCard ZeroCard
translateNum i
    | i < 0 = error "negative ValueNum"
    | i `mod` 2 == 1 = ValueApplication (ValueCard SuccCard) (translateNum (i-1))
    | otherwise = ValueApplication (ValueCard DoubleCard) (translateNum (i `div` 2))

-- (makeK v) is like (K v) but it transforms (K I) into put.
makeK (ValueCard IdentityCard) = ValueCard PutCard
makeK value = ValueApplication (ValueCard KCard) value

-- Build a value from a template.  First argument is a string which is
-- parsed and then passed through translateValue.  Second argument is
-- a set of variable assignments, which are applied to the result of
-- translateValue.
--
-- Variable assignments are also passed through translateValue.
template :: String -> [(String, Value)] -> Value
template s vars = assignVars $ translateValue $ parse s
    where assignVars (ValueCard c) = ValueCard c
          assignVars (ValueApplication f x) = ValueApplication (assignVars f) (assignVars x)
          assignVars (ValueVariable x) = case lookup x vars' of
                                           Nothing -> error ("Unbound variable " ++ x)
                                           Just v -> v
          assignVars (ValueNum x) = ValueNum x
          assignVars _ = error "Internal error: translateValue produced an unexpected value"
          vars' = [(name, translateValue value) | (name, value) <- vars]

test_Translator = [
  translateValue (ValueNum 0) ~?= (ValueNum 0),
  translateValue (ValueNum 1) ~?= (ValueNum 1),
  translateValue (ValueNum 2) ~?= (ValueNum 2),
  translateValue (ValueNum 3) ~?= (ValueNum 3),
  translateValue (ValueNum 4) ~?= (ValueNum 4),
  translateValue (ValueNum 5) ~?= (ValueNum 5),
  translateValue succ ~?= succ,
  translateNum 0 ~?= zero,
  translateNum 1 ~?= app succ zero,
  translateNum 2 ~?= app dbl (app succ zero),
  translateNum 3 ~?= app succ (app dbl (app succ zero)),
  translateNum 4 ~?= app dbl (app dbl (app succ zero)),
  translateNum 5 ~?= app succ (app dbl (app dbl (app succ zero))),
  translateValue (ValueLambda "x" (ValueLambda "y" (ValueVariable "y"))) ~?= ValueCard PutCard,
  translateValue (ValueLambda "x" (ValueVariable "x")) ~?= ValueCard IdentityCard,
  translateValue (ValueLambda "x" (ValueApplication (ValueCard IncCard) (ValueVariable "x"))) ~?= ValueCard IncCard,
  translateValue (ValueLambda "x" (ValueLambda "y" (ValueVariable "x"))) ~?= ValueCard KCard,
  translateValue (ValueLambda "x" (ValueApplication (ValueCard IncCard) (ValueApplication (ValueCard SuccCard) (ValueVariable "x")))) ~?= ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueCard KCard) (ValueCard IncCard))) (ValueCard SuccCard),
  translateValue (ValueLambda "x" (ValueLambda "y" (ValueApplication (ValueApplication (ValueCard PutCard) (ValueVariable "x")) (ValueVariable "y")))) ~?= (ValueCard PutCard),
  translateValue (ValueLambda "x" (ValueLambda "y" (ValueApplication (ValueApplication (ValueCard ZombieCard) (ValueVariable "x")) (ValueVariable "y")))) ~?= (ValueCard ZombieCard),
  translateValue (ValueLambda "bullet" (ValueApplication (ValueApplication (ValueCard PutCard) (ValueApplication (ValueVariable "gun") (ValueVariable "bullet"))) (ValueVariable "value"))) ~?= (ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueCard KCard) (ValueCard PutCard))) (ValueVariable "gun"))) (ValueApplication (ValueCard KCard) (ValueVariable "value"))),
  translateValue (ValueApplication (ValueVariable "lazy") (ValueVariable "x")) ~?= ValueVariable "x",
  (translateValue (ValueLambda "x" (ValueApplication (ValueApplication (ValueCard GetCard) (ValueVariable "x")) (ValueApplication (ValueApplication (ValueVariable "lazy") (ValueApplication (ValueApplication (ValueApplication (ValueCard HelpCard) (ValueNum 0)) (ValueNum 0)) (ValueNum 8196))) (ValueVariable "x"))))
   ~?= (ValueApplication (ValueApplication (ValueCard SCard) (ValueCard GetCard)) (ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueApplication (ValueCard SCard) (ValueApplication (ValueCard KCard) (ValueApplication (ValueApplication (ValueCard HelpCard) (ValueCard ZeroCard)) (ValueCard ZeroCard)))) (ValueApplication (ValueCard KCard) (translateValue (ValueNum 8196))))) (ValueCard IdentityCard)))),
  template "\\x -> x" [] ~?= ValueCard IdentityCard,
  template "K x" [("x", ValueNum 0)] ~?= ValueApplication (ValueCard KCard) (ValueCard ZeroCard),
  template "x K" [("x", ValueCard SCard)] ~?= ValueApplication (ValueCard SCard) (ValueCard KCard)
  ]
    where zero = ValueCard ZeroCard
          succ = ValueCard SuccCard
          dbl = ValueCard DoubleCard
          app = ValueApplication
