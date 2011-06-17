module Strategy(test_Strategy) where

import Test.HUnit
import Value
import Move
import Card

type Slot = Int

-- Replace any instance of ValueNum with an equivalent tree of
-- ValueCards and ValueApplications.
translateNums :: Value -> Value
translateNums (ValueApplication f x) = ValueApplication (translateNums f) (translateNums x)
translateNums (ValueNum 0) = ValueCard ZeroCard
translateNums (ValueNum i) | i < 0 = error "negative value in translateNums"
                           | i `mod` 2 == 1 = ValueApplication (ValueCard SuccCard) (translateNums (ValueNum (i-1)))
                           | otherwise = ValueApplication (ValueCard DoubleCard) (translateNums (ValueNum (i `div` 2)))
translateNums (ValueCard c) = ValueCard c

-- Determine whether the given value is in "vine" form.  "Vine" form
-- requires that:
-- - There are no ValueNums anywhere in the tree
-- - Every application has a ValueCard for either its LHS or RHS.
isVine :: Value -> Bool
isVine (ValueCard _) = True
isVine (ValueNum _) = False
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
isRightVine (ValueNum _) = False
isRightVine (ValueApplication (ValueCard _) v) = isRightVine v
isRightVine (ValueApplication _ _) = False

-- Build a value which satisfies the isVine predicate.
-- Assumes:
-- - the slot previously held the identity function.
buildVine :: Slot -> Value -> [Move]
buildVine slot vine = buildVine' vine []
    where buildVine' (ValueCard c) = (Move RightApplication c slot :)
          buildVine' (ValueNum _) = error "call translateNums before buildVine"
          buildVine' (ValueApplication (ValueCard c) v) = buildVine' v . (Move LeftApplication c slot :)
          buildVine' (ValueApplication v (ValueCard c)) = buildVine' v . (Move RightApplication c slot :)
          buildVine' (ValueApplication v w) = error "buildVine': not a vine"

-- Apply the value (which must satisfy the isRightVine predicate) to
-- the contents of the given slot.
applyRightVine :: Slot -> Value -> [Move]
applyRightVine slot (ValueCard c) = [Move RightApplication c slot]
applyRightVine slot (ValueApplication (ValueCard c) v)
    = [Move LeftApplication KCard slot, Move LeftApplication SCard slot, Move RightApplication c slot]
      ++ applyRightVine slot v
applyRightVine _ _ = error "applyRightVine: not a right vine"

-- Build a value of the form (ValueApplication v rv), where v
-- satisfies the isVine predicate, and rv satisfies the isRightVine
-- predicate.
-- Assumes:
-- - the slot previously held the identity function.
buildVrv :: Slot -> Value -> [Move]
buildVrv slot (ValueApplication v rv) = buildVine slot v ++ applyRightVine slot rv
buildVrv slot _ = error "buildVrv: not a ValueApplication"

test_Strategy = [
  translateNums (ValueNum 0) ~?= zero,
  translateNums (ValueNum 1) ~?= app succ zero,
  translateNums (ValueNum 2) ~?= app dbl (app succ zero),
  translateNums (ValueNum 3) ~?= app succ (app dbl (app succ zero)),
  translateNums (ValueNum 4) ~?= app dbl (app dbl (app succ zero)),
  translateNums (ValueNum 5) ~?= app succ (app dbl (app dbl (app succ zero))),
  translateNums succ ~?= succ,
  translateNums (app (ValueNum 1) (ValueNum 2)) ~?= app (app succ zero) (app dbl (app succ zero)),
  isVine dbl ~?= True,
  isVine (ValueNum 1) ~?= False,
  isVine (ValueApplication succ zero) ~?= True,
  isVine (ValueApplication succ (ValueApplication succ zero)) ~?= True,
  isVine (ValueApplication (ValueApplication k succ) zero) ~?= True,
  isVine (ValueApplication (ValueApplication k succ) (ValueApplication k succ)) ~?= False,
  isRightVine dbl ~?= True,
  isRightVine (ValueNum 1) ~?= False,
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
  applyRightVine 10 zero ~?= [Move RightApplication ZeroCard 10],
  (applyRightVine 10 (ValueApplication succ zero)
   ~?= [Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication SuccCard 10,
        Move RightApplication ZeroCard 10]),
  (applyRightVine 10 (ValueApplication dbl (ValueApplication succ zero))
   ~?= [Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication DoubleCard 10,
        Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication SuccCard 10,
        Move RightApplication ZeroCard 10]), -- TODO: I'm not certain this is correct --Paul
  buildVrv 10 (ValueApplication succ zero) ~?= [Move RightApplication SuccCard 10, Move RightApplication ZeroCard 10],
  (buildVrv 10 (ValueApplication (ValueApplication (ValueApplication s (ValueApplication k get)) get)
                (ValueApplication succ zero)) -- get(get(1))
   ~?= [Move RightApplication GetCard 10, Move LeftApplication KCard 10, Move LeftApplication SCard 10,
        Move RightApplication GetCard 10, Move LeftApplication KCard 10, Move LeftApplication SCard 10,
        Move RightApplication SuccCard 10, Move RightApplication ZeroCard 10])
  ] :: [Test]
    where zero = ValueCard ZeroCard
          succ = ValueCard SuccCard
          dbl = ValueCard DoubleCard
          k = ValueCard KCard
          app = ValueApplication
          s = ValueCard SCard
          get = ValueCard GetCard
