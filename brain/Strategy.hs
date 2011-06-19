module Strategy(test_Strategy, buildValue, translateValue, makeK, template) where

import Control.Monad.State
import Test.HUnit ( (~?=) )
import Value
import Move
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
    where expandMacro (ValueNum 0) = ValueCard ZeroCard
          expandMacro (ValueNum i)
              | i < 0 = error "negative ValueNum"
              | i `mod` 2 == 1 = ValueApplication (ValueCard SuccCard) (expandMacro (ValueNum (i-1)))
              | otherwise = ValueApplication (ValueCard DoubleCard) (expandMacro (ValueNum (i `div` 2)))
          expandMacro (ValueLambda varName (ValueVariable x)) | varName == x = ValueCard IdentityCard
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

-- Eventually translateValue will only translate things other than
-- numbers, and translateNum will only translate numbers.  For now
-- they do the same thing.
translateNum = translateValue

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
          assignVars _ = error "Internal error: translateValue produced an unexpected value"
          vars' = [(name, translateValue value) | (name, value) <- vars]

-- Determine whether the given value is in "vine" form.  "Vine" form
-- requires that:
-- - There are no ValueNums anywhere in the tree
-- - Every application has a ValueCard for either its LHS or RHS.
isVine :: Value -> Bool
isVine (ValueCard _) = True
isVine (ValueNum _) = True
isVine (ValueApplication (ValueCard _) v) = isVine v
isVine (ValueApplication v (ValueCard _)) = isVine v
isVine (ValueApplication _ _) = False

-- Determine whether the given value is in "right vine" form.  "Right
-- vine" form requires that:
-- - There are no ValueNums anywhere in the tree
-- - Every application has a ValueCard for its LHS.
-- That is, the "vine" grows exclusively downward and to the right.
isRightVine :: Value -> Bool
isRightVine (ValueCard _) = True
isRightVine (ValueNum _) = True
isRightVine (ValueApplication (ValueCard _) v) = isRightVine v
isRightVine (ValueApplication _ _) = False

-- Build a value which satisfies the isVine predicate.
-- Assumes:
-- - the slot previously held the identity function.
buildVine :: SlotNumber -> Value -> [Move]
buildVine slot vine = buildVine' vine []
    where buildVine' (ValueCard c) = (Move RightApplication c slot :)
          buildVine' v@(ValueNum _) = buildVine' (translateNum v)
          buildVine' (ValueApplication (ValueCard c) v) = buildVine' v . (Move LeftApplication c slot :)
          buildVine' (ValueApplication v (ValueCard c)) = buildVine' v . (Move RightApplication c slot :)
          buildVine' (ValueApplication v w) = error "buildVine': not a vine"

-- Apply the value (which must satisfy the isRightVine predicate) to
-- the contents of the given slot.
applyRightVine :: SlotNumber -> Value -> [Move]
applyRightVine slot (ValueCard c) = [Move RightApplication c slot]
applyRightVine slot (ValueApplication (ValueCard c) v)
    = [Move LeftApplication KCard slot, Move LeftApplication SCard slot, Move RightApplication c slot]
      ++ applyRightVine slot v
applyRightVine slot v@(ValueNum _) = applyRightVine slot (translateNum v)
applyRightVine _ _ = error "applyRightVine: not a right vine"

-- Build a value of the form (ValueApplication v rv), where v
-- satisfies the isVine predicate, and rv satisfies the isRightVine
-- predicate.
-- Assumes:
-- - the slot previously held the identity function.
buildVrv :: SlotNumber -> Value -> [Move]
buildVrv slot (ValueApplication v rv) = buildVine slot v ++ applyRightVine slot rv
buildVrv slot _ = error "buildVrv: not a ValueApplication"

-- Build a general value.  Requires temporary slots.
-- Assumes:
-- - the destination slot and all temporary slots currently hold the identity function.
buildValue :: SlotNumber -> Value -> State [SlotNumber] [Move]
buildValue destSlot v | isVine v = return $ buildVine destSlot v
buildValue destSlot (ValueApplication f x)
    | isRightVine x = do moves1 <- buildValue destSlot f
                         let moves2 = applyRightVine destSlot x
                         return $ moves1 ++ moves2
    | otherwise = do moves1 <- buildValue destSlot f
                     availableSlots <- get
                     let slotToUse = head availableSlots
                     put $ tail availableSlots
                     moves2 <- buildValue slotToUse x
                     let moves3 = applyRightVine destSlot $ translateValue $ ValueApplication (ValueCard GetCard) (ValueNum slotToUse)
                     return $ moves1 ++ moves2 ++ moves3

test_Strategy = [
  translateValue (ValueNum 0) ~?= zero,
  translateValue (ValueNum 1) ~?= app succ zero,
  translateValue (ValueNum 2) ~?= app dbl (app succ zero),
  translateValue (ValueNum 3) ~?= app succ (app dbl (app succ zero)),
  translateValue (ValueNum 4) ~?= app dbl (app dbl (app succ zero)),
  translateValue (ValueNum 5) ~?= app succ (app dbl (app dbl (app succ zero))),
  translateValue succ ~?= succ,
  translateValue (app (ValueNum 1) (ValueNum 2)) ~?= app (app succ zero) (app dbl (app succ zero)),
  translateNum (ValueNum 0) ~?= zero,
  translateNum (ValueNum 1) ~?= app succ zero,
  translateNum (ValueNum 2) ~?= app dbl (app succ zero),
  translateNum (ValueNum 3) ~?= app succ (app dbl (app succ zero)),
  translateNum (ValueNum 4) ~?= app dbl (app dbl (app succ zero)),
  translateNum (ValueNum 5) ~?= app succ (app dbl (app dbl (app succ zero))),
  isVine dbl ~?= True,
  isVine (ValueNum 1) ~?= True,
  isVine (ValueApplication succ zero) ~?= True,
  isVine (ValueApplication succ (ValueApplication succ zero)) ~?= True,
  isVine (ValueApplication (ValueApplication k succ) zero) ~?= True,
  isVine (ValueApplication (ValueApplication k succ) (ValueApplication k succ)) ~?= False,
  isRightVine dbl ~?= True,
  isRightVine (ValueNum 1) ~?= True,
  isRightVine (ValueApplication succ zero) ~?= True,
  isRightVine (ValueApplication succ (ValueApplication succ zero)) ~?= True,
  isRightVine (ValueApplication (ValueApplication k succ) zero) ~?= False,
  isRightVine (ValueApplication (ValueApplication k succ) (ValueApplication k succ)) ~?= False,
  buildVine 10 dbl ~?= [Move RightApplication DoubleCard 10],
  buildVine 10 (ValueApplication succ zero) ~?= [Move RightApplication ZeroCard 10, Move LeftApplication SuccCard 10],
  (buildVine 10 (ValueApplication succ (ValueApplication succ zero))
   ~?= [Move RightApplication ZeroCard 10, Move LeftApplication SuccCard 10, Move LeftApplication SuccCard 10]),
  (buildVine 10 (ValueApplication (ValueApplication k succ) zero)
   ~?= [Move RightApplication SuccCard 10, Move LeftApplication KCard 10, Move RightApplication ZeroCard 10]),
  (buildVine 10 (ValueNum 3) ~?= buildVine 10 (translateNum $ ValueNum 3)),
  applyRightVine 10 zero ~?= [Move RightApplication ZeroCard 10],
  (applyRightVine 10 (ValueApplication succ zero)
   ~?= [Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication SuccCard 10,
        Move RightApplication ZeroCard 10]),
  (applyRightVine 10 (ValueApplication dbl (ValueApplication succ zero))
   ~?= [Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication DoubleCard 10,
        Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication SuccCard 10,
        Move RightApplication ZeroCard 10]),
  (applyRightVine 10 (ValueNum 5) ~?= applyRightVine 10 (translateNum (ValueNum 5))),
  buildVrv 10 (ValueApplication succ zero) ~?= [Move RightApplication SuccCard 10, Move RightApplication ZeroCard 10],
  (buildVrv 10 (ValueApplication (ValueApplication (ValueApplication s (ValueApplication k get)) get)
                (ValueApplication succ zero)) -- get(get(1))
   ~?= [Move RightApplication GetCard 10, Move LeftApplication KCard 10, Move LeftApplication SCard 10,
        Move RightApplication GetCard 10, Move LeftApplication KCard 10, Move LeftApplication SCard 10,
        Move RightApplication SuccCard 10, Move RightApplication ZeroCard 10]),
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
          k = ValueCard KCard
          app = ValueApplication
          s = ValueCard SCard
          get = ValueCard GetCard
