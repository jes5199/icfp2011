module Strategy(test_Strategy, buildValue, applyRightVine, isVine, isRightVine) where

import Control.Monad.State
import Test.HUnit ( (~?=) )
import Value
import Move
import Card
--import Parser
import Translator

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
          buildVine' (ValueNum v) = buildVine' (translateNum v)
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
applyRightVine slot (ValueNum v) = applyRightVine slot (translateNum v)
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
  (buildVine 10 (ValueNum 3) ~?= buildVine 10 (translateNum 3)),
  applyRightVine 10 zero ~?= [Move RightApplication ZeroCard 10],
  (applyRightVine 10 (ValueApplication succ zero)
   ~?= [Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication SuccCard 10,
        Move RightApplication ZeroCard 10]),
  (applyRightVine 10 (ValueApplication dbl (ValueApplication succ zero))
   ~?= [Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication DoubleCard 10,
        Move LeftApplication KCard 10, Move LeftApplication SCard 10, Move RightApplication SuccCard 10,
        Move RightApplication ZeroCard 10]),
  (applyRightVine 10 (ValueNum 5) ~?= applyRightVine 10 (translateNum 5)),
  buildVrv 10 (ValueApplication succ zero) ~?= [Move RightApplication SuccCard 10, Move RightApplication ZeroCard 10],
  (buildVrv 10 (ValueApplication (ValueApplication (ValueApplication s (ValueApplication k get)) get)
                (ValueApplication succ zero)) -- get(get(1))
   ~?= [Move RightApplication GetCard 10, Move LeftApplication KCard 10, Move LeftApplication SCard 10,
        Move RightApplication GetCard 10, Move LeftApplication KCard 10, Move LeftApplication SCard 10,
        Move RightApplication SuccCard 10, Move RightApplication ZeroCard 10])
  ]
    where zero = ValueCard ZeroCard
          succ = ValueCard SuccCard
          dbl = ValueCard DoubleCard
          k = ValueCard KCard
          s = ValueCard SCard
          get = ValueCard GetCard
