module Main where

import Strategy
import Value
import Card
import Move
import Parser
import System(getArgs)
import Control.Monad.Writer.Strict
import Control.Monad.State

type SlotNum = Int

type TestCaseGenerator = StateT [SlotNum] (Writer [Move])

buildNewValue :: Value -> TestCaseGenerator SlotNum
buildNewValue value = do
  (destSlot : availSlots) <- get
  let (moves, availSlots') = runState (buildValue destSlot (translateNums $ translateLambda value)) availSlots
  tell moves
  put availSlots'
  return destSlot

rightApply :: SlotNum -> Card -> TestCaseGenerator ()
rightApply slot card = tell [Move RightApplication card slot]

testCycleCount :: Int -> TestCaseGenerator ()
testCycleCount wasteCycles = do
  gun <- buildNewValue (parse "\\x -> put (inc (succ x)) (get x x)")
  let triggerValue = waste wasteCycles (translateLambda $ parse "\\x -> get x x")
  trigger <- buildNewValue triggerValue
  rightApply trigger ZeroCard
    where waste 0 value = value
          waste n value = ValueApplication (ValueApplication (ValueCard SCard) (waste (n-1) value))
                                           (ValueCard IdentityCard)

testCase = testCycleCount 1

main :: IO ()
main = do
  let moves = execWriter (evalStateT testCase [0..255])
  putStr (printMoves moves)
