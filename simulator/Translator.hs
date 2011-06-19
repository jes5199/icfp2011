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
    where expandMacro (ValueLambda varName (ValueVariable x)) | varName == x = valueI
          expandMacro (ValueLambda varName value) | not (value `includes` varName) = makeK value
          expandMacro (ValueLambda varName (ValueApplication f (ValueVariable x)))
              | varName == x && not (f `includes` varName) = f
          expandMacro (ValueLambda varName (ValueApplication f x))
              = ValueApplication (ValueApplication valueS (expandMacro (ValueLambda varName f)))
                                 (expandMacro (ValueLambda varName x))
          expandMacro (ValueApplication (ValueVariable "lazy") value) = lazify value
          expandMacro value = value
          ValueVariable x `includes` varName = x == varName
          ValueApplication f x `includes` varName = (f `includes` varName) || (x `includes` varName)
          ValueLambda varNameInner value `includes` varName | varNameInner == varName = False
                                                            | otherwise = value `includes` varName
          _ `includes` varName = False
          lazify (ValueApplication f x) = ValueApplication (ValueApplication valueS (ValueApplication (ValueApplication valueS (makeK f)) (makeK x))) valueI
          lazify value = value

-- Like translateValue, but translate numbers to sequences of succ, dbl, and zero.
translateNum 0 = valueZero
translateNum i
    | i < 0 = error "negative ValueNum"
    | i `mod` 2 == 1 = ValueApplication valueSucc (translateNum (i-1))
    | otherwise = ValueApplication valueDbl (translateNum (i `div` 2))

-- (makeK v) is like (K v) but it transforms (K I) into put.
makeK (ValueCard IdentityCard) = valuePut
makeK value = ValueApplication valueK value

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
  translateValue (num 0) ~?= (num 0),
  translateValue (num 1) ~?= (num 1),
  translateValue (num 2) ~?= (num 2),
  translateValue (num 3) ~?= (num 3),
  translateValue (num 4) ~?= (num 4),
  translateValue (num 5) ~?= (num 5),
  translateValue succ ~?= succ,
  translateNum 0 ~?= zero,
  translateNum 1 ~?= app succ zero,
  translateNum 2 ~?= app dbl (app succ zero),
  translateNum 3 ~?= app succ (app dbl (app succ zero)),
  translateNum 4 ~?= app dbl (app dbl (app succ zero)),
  translateNum 5 ~?= app succ (app dbl (app dbl (app succ zero))),
  translateValue (lambda "x" (lambda "y" (var "y"))) ~?= valuePut,
  translateValue (lambda "x" (var "x")) ~?= valueI,
  translateValue (lambda "x" (app valueInc (var "x"))) ~?= valueInc,
  translateValue (lambda "x" (lambda "y" (var "x"))) ~?= ValueCard KCard,
  translateValue (lambda "x" (app valueInc (app valueSucc (var "x")))) ~?= app (app valueS (app valueK valueInc)) valueSucc,
  translateValue (lambda "x" (lambda "y" (app (app valuePut (var "x")) (var "y")))) ~?= valuePut,
  translateValue (lambda "x" (lambda "y" (app (app valueZombie (var "x")) (var "y")))) ~?= valueZombie,
  translateValue (lambda "bullet" (app (app valuePut (app (var "gun") (var "bullet"))) (var "value"))) ~?= (app (app valueS (app (app valueS (app valueK valuePut)) (var "gun"))) (app valueK (var "value"))),
  translateValue (app (var "lazy") (var "x")) ~?= var "x",
  (translateValue (lambda "x" (app (app valueGet (var "x")) (app (app (var "lazy") (app (app (app valueHelp (num 0)) (num 0)) (num 8196))) (var "x"))))
   ~?= (app (app valueS valueGet) (app (app valueS (app (app valueS (app valueK (app (app valueHelp valueZero) valueZero))) (app valueK (translateValue (num 8196))))) valueI))),
  template "\\x -> x" [] ~?= valueI,
  template "K x" [("x", num 0)] ~?= app valueK valueZero,
  template "x K" [("x", valueS)] ~?= app valueS valueK
  ]
    where zero = ValueCard ZeroCard
          succ = ValueCard SuccCard
          dbl = ValueCard DoubleCard
          app = ValueApplication
          var = ValueVariable
          lambda = ValueLambda
          num = ValueNum
