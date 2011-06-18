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
import Statements

type TestCaseGenerator = StateT [SlotNumber] (Writer [Move])

buildNewValue :: Value -> TestCaseGenerator SlotNumber
buildNewValue value = do
  (destSlot : availSlots) <- get
  let (moves, availSlots') = runState (buildValue destSlot (translateValue value)) availSlots
  tell moves
  put availSlots'
  return destSlot

buildNewValueAt :: Value -> SlotNumber -> TestCaseGenerator SlotNumber
buildNewValueAt value destSlot = do
  slots <- get
  case elem destSlot slots of
    False -> error "Slot not available"
    True -> do let availSlots = filter (/= destSlot) slots
                   (moves, availSlots') = runState (buildValue destSlot (translateValue value)) availSlots
               tell moves
               put availSlots'
               return destSlot

rightApply :: SlotNumber -> Card -> TestCaseGenerator ()
rightApply slot card = tell [Move RightApplication card slot]

testCycleCount :: Int -> TestCaseGenerator ()
testCycleCount wasteCycles = do
  gun <- buildNewValue (parse "\\x -> put (inc (succ x)) (get x x)")
  let triggerValue = waste wasteCycles (translateValue $ parse "\\x -> get x x")
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
              return ()),
 ("attack_KKK", do buildNewValue (parse "attack K K K")
                   return ()),
 ("attack_0KK", do buildNewValue (parse "attack 0 K K")
                   return ()),
 ("attack_KK0", do buildNewValue (parse "attack K K 0")
                   return ()),
 ("attack_0K0", do buildNewValue (parse "attack 0 K 0")
                   return ()),
 ("attack_0K1", do buildNewValue (parse "attack 0 K 1")
                   return ()),
 ("attack_1K1", do buildNewValue (parse "attack 1 K 1")
                   return ()),
 ("attack_255K1", do buildNewValue (parse "attack 255 K 1")
                     return ()),
 ("attack_256K1", do buildNewValue (parse "attack 256 K 1")
                     return ()),
 ("attack_101", do buildNewValue (parse "attack 1 0 1")
                   return ()),
 ("attack_111", do buildNewValue (parse "attack 1 1 1")
                   return ()),
 ("attack_112", do buildNewValue (parse "attack 1 1 2")
                   return ()),
 ("attack_1.255.2", do buildNewValue (parse "attack 1 255 2")
                       return ()),
 ("attack_1.256.2", do buildNewValue (parse "attack 1 256 2")
                       return ()),
 ("attack_119", do buildNewValue (parse "attack 1 1 9")
                   return ()),
 ("attack_11.10", do buildNewValue (parse "attack 1 1 10")
                     return ()),
 ("attack_11.11", do buildNewValue (parse "attack 1 1 11")
                     return ()),
 ("attack_11.10001", do buildNewValue (parse "attack 1 1 10001")
                        return ()),
 ("help_KKK", do buildNewValue (parse "help K K K")
                 return ()),
 ("help_0KK", do buildNewValue (parse "help 0 K K")
                 return ()),
 ("help_KK0", do buildNewValue (parse "help K K 0")
                 return ()),
 ("help_0K0", do buildNewValue (parse "help 0 K 0")
                 return ()),
 ("help_0K1", do buildNewValue (parse "help 0 K 1")
                 return ()),
 ("help_1K1", do buildNewValue (parse "help 1 K 1")
                 return ()),
 ("help_255K1", do buildNewValue (parse "help 255 K 1")
                   return ()),
 ("help_256K1", do buildNewValue (parse "help 256 K 1")
                   return ()),
 ("help_101", do buildNewValue (parse "help 1 0 1")
                 return ()),
 ("help_111", do buildNewValue (parse "help 1 1 1")
                 return ()),
 ("help_112", do buildNewValue (parse "help 1 1 2")
                 return ()),
 ("help_1.255.2", do buildNewValue (parse "help 1 255 2")
                     return ()),
 ("help_1.256.2", do buildNewValue (parse "help 1 256 2")
                     return ()),
 ("help_119", do buildNewValue (parse "help 1 1 9")
                 return ()),
 ("help_11.10", do buildNewValue (parse "help 1 1 10")
                   return ()),
 ("help_11.11", do buildNewValue (parse "help 1 1 11")
                   return ()),
 ("help_11.10001", do buildNewValue (parse "help 1 1 10001")
                      return ()),
 ("copyK", do buildNewValue (parse "copy K")
              return ()),
 ("copy0", do buildNewValue (parse "copy 0")
              return ()),
 ("copy255", do buildNewValue (parse "copy 255")
                return ()),
 ("copy256", do buildNewValue (parse "copy 256")
                return ()),
 ("reviveinc", do buildNewValue (parse "revive inc")
                  return ()),
 ("revive0", do buildNewValue (parse "revive 0")
                return ()),
 ("revive255", do buildNewValue (parse "revive 255")
                  return ()),
 ("revive256", do buildNewValue (parse "revive 256")
                  return ()),
 ("use_dead", do buildNewValue (parse "help 200 0 10000")
                 buildNewValueAt (parse "S") 200
                 return ()),
 ("use_dead2", do buildNewValueAt (parse "K") 200
                  buildNewValue (parse "help 200 0 10000")
                  rightApply 200 SCard
                  return ()),
 ("lazy", do loc <- buildNewValue (parse "lazy (inc 0)")
             rightApply loc ZombieCard),
 ("health_bomb", do loc <- buildNewValue (parse "\\x -> get x (lazy (help 0 0 8196) x)")
                    rightApply loc ZeroCard),
 ("get_from_dead", do buildNewValueAt (parse "S") 200
                      buildNewValue (parse "help 200 0 10000")
                      buildNewValue (parse "get 200")
                      return ()),
 ("grapeshot", do buildNewValueAt (grapeShot 8192 0) 0
                  rightApply 0 ZeroCard)
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
