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

buildNewValueAt :: Value -> SlotNum -> TestCaseGenerator SlotNum
buildNewValueAt value destSlot = do
  slots <- get
  case elem destSlot slots of
    False -> error "Slot not available"
    True -> do let availSlots = filter (/= destSlot) slots
                   (moves, availSlots') = runState (buildValue destSlot (translateNums $ translateLambda value)) availSlots
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
                rightApply triggerLoc ZeroCard),
 ("basic_inc", do buildNewValue (parse "inc 5")
                  return ()),
 ("inc_out_of_range", do buildNewValue (parse "inc 256")
                         return ()),
 ("I5", do buildNewValue (parse "I I I I I")
           return ()),
 ("I5_3", do buildNewValue (parse "I I I I I 3")
             return ()),
 ("65534", do buildNewValue (parse "65534")
              return ()),
 ("65535", do buildNewValue (parse "65535")
              return ()),
 ("65536", do buildNewValue (parse "65536")
              return ()),
 ("get255", do buildNewValueAt (parse "S") 255
               buildNewValue (parse "get 255")
               return ()),
 ("get0", do buildNewValueAt (parse "K") 0
             buildNewValue (parse "get 0")
             return ()),
 ("get256", do buildNewValue (parse "get 256")
               return ()),
 ("double0", do buildNewValue (parse "dbl 0")
                return ()),
 ("double32767", do buildNewValue (parse "dbl 32767")
                    return ()),
 ("double32768", do buildNewValue (parse "dbl 32768")
                    return ()),
 ("SKSSSK", do buildNewValue (parse "S(K)(S(S)(S(K)))")
               return ()),
 ("put12", do buildNewValue (parse "put 1 2")
              return ()),
 ("inc0", do buildNewValue (parse "inc 0")
             return ()),
 ("inc1", do buildNewValue (parse "inc 1")
             return ()),
 ("inc255", do buildNewValue (parse "inc 255")
               return ()),
 ("incK", do buildNewValue (parse "inc K")
             return ()),
 ("inc256", do buildNewValue (parse "inc 256")
               return ()),
 ("inc_maxed", do buildNewValueAt (parse "S") 100
                  buildNewValueAt (parse "S") 101
                  buildNewValueAt (parse "S") 102
                  buildNewValueAt (parse "S") 103
                  buildNewValueAt (parse "S") 104
                  buildNewValueAt (parse "S") 105
                  buildNewValueAt (parse "K") 200
                  buildNewValue (parse "help 100 200 10000")
                  buildNewValue (parse "help 101 200 10000")
                  buildNewValue (parse "help 102 200 10000")
                  buildNewValue (parse "help 103 200 10000")
                  buildNewValue (parse "help 104 200 10000")
                  buildNewValue (parse "help 105 200 10000")
                  buildNewValue (parse "inc 200")
                  return ()),
 ("inc_dead", do buildNewValueAt (parse "S") 100
                 buildNewValue (parse "help 100 200 10000")
                 buildNewValue (parse "inc 100")
                 return ()),
 ("inc_65534", do buildNewValueAt (parse "S") 100
                  buildNewValueAt (parse "S") 101
                  buildNewValueAt (parse "S") 102
                  buildNewValueAt (parse "S") 103
                  buildNewValueAt (parse "S") 104
                  buildNewValueAt (parse "S") 105
                  buildNewValueAt (parse "K") 200
                  buildNewValue (parse "help 100 200 10000")
                  buildNewValue (parse "help 101 200 10000")
                  buildNewValue (parse "help 102 200 10000")
                  buildNewValue (parse "help 103 200 10000")
                  buildNewValue (parse "help 104 200 10000")
                  buildNewValue (parse "help 105 200 486")
                  buildNewValue (parse "inc 200")
                  return ()),
 ("inc_1", do buildNewValueAt (parse "S") 100
              buildNewValue (parse "help 100 200 10000")
              buildNewValue (parse "revive 100")
              buildNewValue (parse "inc 100")
              return ()),
 ("dec0", do buildNewValue (parse "dec 0")
             return ()),
 ("dec1", do buildNewValue (parse "dec 1")
             return ()),
 ("dec255", do buildNewValue (parse "dec 255")
               return ()),
 ("decK", do buildNewValue (parse "dec K")
             return ()),
 ("dec256", do buildNewValue (parse "dec 256")
               return ()),
 ("dec_dead", do buildNewValueAt (parse "S") 100
                 buildNewValueAt (parse "S") 101
                 buildNewValue (parse "attack 100 200 10000")
                 buildNewValue (parse "attack 101 200 10000")
                 buildNewValue (parse "dec 200")
                 return ()),
 ("dec_1", do buildNewValueAt (parse "S") 100
              buildNewValueAt (parse "S") 101
              buildNewValue (parse "attack 100 200 10000")
              buildNewValue (parse "attack 101 200 1110")
              buildNewValue (parse "dec 200")
              return ())
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
