module Main where

import Strategy
import Value
import Card
import Move
import Parser
import System(getArgs)
import Control.Monad.Writer.Strict
import Control.Monad.State
import Data.List

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

testCases :: [(String, TestCaseGenerator ())]
testCases = [
 ("cycle_count_91", testCycleCount 0),
 ("cycle_count_90", testCycleCount 1),
 ("get_inc", do 0 <- buildNewValue (parse "inc")
                triggerLoc <- buildNewValue (parse "S get I")
                rightApply triggerLoc ZeroCard)
 ]

outputTestCase :: String -> TestCaseGenerator () -> IO ()
outputTestCase testName testCase = do
  [directory] <- getArgs
  let moves = execWriter (evalStateT testCase [0..255])
      fileContents = printMoves (concat [[move, Move LeftApplication IdentityCard 0] | move <- moves])
  writeFile (directory ++ "/" ++ testName) fileContents

main :: IO ()
main = do
  putStrLn "Generating test cases..."
  sequence_ $ do (testName, testCase) <- testCases
                 return (outputTestCase testName testCase)
  putStrLn "Done."
