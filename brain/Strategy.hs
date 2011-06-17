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

-- Build a value in which every application has a leaf node on either
-- its LHS or RHS.
-- Assumes:
-- - the slot previously held the identity function.
-- - the vine contains no ValueNum's (see translateNums).
buildVine :: Slot -> Value -> [Move]
buildVine slot vine = buildVine' vine []
    where buildVine' (ValueCard c) = (Move RightApplication c slot :)
          buildVine' (ValueNum _) = error "call translateNums before buildVine"
          buildVine' (ValueApplication (ValueCard c) v) = buildVine' v . (Move LeftApplication c slot :)
          buildVine' (ValueApplication v (ValueCard c)) = buildVine' v . (Move RightApplication c slot :)
          buildVine' (ValueApplication v w) = error "buildVine': not a vine"

test_Strategy = [
  translateNums (ValueNum 0) ~?= zero,
  translateNums (ValueNum 1) ~?= app succ zero,
  translateNums (ValueNum 2) ~?= app dbl (app succ zero),
  translateNums (ValueNum 3) ~?= app succ (app dbl (app succ zero)),
  translateNums (ValueNum 4) ~?= app dbl (app dbl (app succ zero)),
  translateNums (ValueNum 5) ~?= app succ (app dbl (app dbl (app succ zero))),
  translateNums succ ~?= succ,
  translateNums (app (ValueNum 1) (ValueNum 2)) ~?= app (app succ zero) (app dbl (app succ zero)),
  buildVine 10 dbl ~?= [Move RightApplication DoubleCard 10],
  buildVine 10 (ValueApplication succ zero) ~?= [Move RightApplication ZeroCard 10, Move LeftApplication SuccCard 10],
  (buildVine 10 (ValueApplication succ (ValueApplication succ zero))
   ~?= [Move RightApplication ZeroCard 10, Move LeftApplication SuccCard 10, Move LeftApplication SuccCard 10]),
  (buildVine 10 (ValueApplication (ValueApplication k succ) zero)
   ~?= [Move RightApplication SuccCard 10, Move LeftApplication KCard 10, Move RightApplication ZeroCard 10])
  ] :: [Test]
    where zero = ValueCard ZeroCard
          succ = ValueCard SuccCard
          dbl = ValueCard DoubleCard
          k = ValueCard KCard
          app = ValueApplication
