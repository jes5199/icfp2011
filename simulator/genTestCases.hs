module Main where

import SimpleBuilder
import Translator
import Value
import Card
import Move
import Parser
import System(getArgs)
import Control.Monad.Writer.Strict
import Control.Monad.State
import Data.List
import Statements
import GameState
import Simulator
import MoveWriter(MoveWriter,execMoveWriter)
import qualified KillerOf255

type TestCaseGenerator = StateT ([SlotNumber], [SlotNumber], Who, GameState) (Writer [TestCaseAtom])

data TestCaseAtom = TestCaseMove Move
                  | TestCaseAssertionFailure GameState String

assert :: String -> (GameState -> Bool) -> TestCaseGenerator ()
assert msg f = do
  (_, _, _, gs) <- get
  case f gs of
    True -> return ()
    False -> tell [TestCaseAssertionFailure gs msg]

assertProponent :: String -> (GSPerspective -> GameState -> Bool) -> TestCaseGenerator ()
assertProponent msg f = do
  who <- getProponent
  assert msg (f (perspectiveFor who False))

assertOpponent :: String -> (GSPerspective -> GameState -> Bool) -> TestCaseGenerator ()
assertOpponent msg f = do
  who <- getProponent
  assert msg (f (perspectiveFor (opponent who) False))

getProponent :: TestCaseGenerator Who
getProponent = do
  (_, _, who, _) <- get
  return who

getPerspective :: TestCaseGenerator GSPerspective
getPerspective = do
  who <- getProponent
  return (perspectiveFor who False)

getProponentAvailSlots :: TestCaseGenerator [SlotNumber]
getProponentAvailSlots = do
  (p1Slots, p2Slots, who, _) <- get
  case who of
    FirstPlayer -> return p1Slots
    SecondPlayer -> return p2Slots

putProponentAvailSlots :: [SlotNumber] -> TestCaseGenerator ()
putProponentAvailSlots newSlots = do
  (p1Slots, p2Slots, who, gs) <- get
  case who of
    FirstPlayer -> put (newSlots, p2Slots, who, gs)
    SecondPlayer -> put (p1Slots, newSlots, who, gs)

tellMove :: Move -> TestCaseGenerator ()
tellMove move = do
  tell [TestCaseMove move]
  (p1Slots, p2Slots, who, gs) <- get
  put (p1Slots, p2Slots, who, simulate gs move)

ensureOurMove :: TestCaseGenerator ()
ensureOurMove = do
  (_, _, who, gs) <- get
  (if who == playerToMove gs
   then return ()
   else tellMove nullMove)

makeMove :: Move -> TestCaseGenerator ()
makeMove move = do
  ensureOurMove
  tellMove move

makeMoves :: [Move] -> TestCaseGenerator ()
makeMoves = mapM_ makeMove

switchPlayers :: TestCaseGenerator ()
switchPlayers = do
  (p1Slots, p2Slots, who, gs) <- get
  put (p1Slots, p2Slots, opponent who, gs)

rightVineBuild :: Int -> Value -> TestCaseGenerator ()
rightVineBuild slotNum value = makeMoves $ applyRightVine slotNum value

buildNewValue :: Value -> TestCaseGenerator SlotNumber
buildNewValue value = do
  who <- getProponent
  (destSlot : availSlots) <- getProponentAvailSlots
  let (moves, availSlots') = runState (buildValue destSlot (translateValue value)) availSlots
  makeMoves moves
  putProponentAvailSlots availSlots'
  return destSlot

buildNewValueAt :: Value -> SlotNumber -> TestCaseGenerator SlotNumber
buildNewValueAt value destSlot = do
  slots <- getProponentAvailSlots
  case elem destSlot slots of
    False -> error "Slot not available"
    True -> do let availSlots = filter (/= destSlot) slots
                   (moves, availSlots') = runState (buildValue destSlot (translateValue value)) availSlots
               makeMoves moves
               putProponentAvailSlots availSlots'
               return destSlot

rightApply :: SlotNumber -> Card -> TestCaseGenerator ()
rightApply slot card = makeMoves [Move RightApplication card slot]

leftApply :: SlotNumber -> Card -> TestCaseGenerator ()
leftApply slot card = makeMoves [Move LeftApplication card slot]

testCycleCount :: Int -> TestCaseGenerator ()
testCycleCount wasteCycles = do
  gun <- buildNewValue (parse "\\x -> put (inc (succ x)) (get x x)")
  let triggerValue = waste wasteCycles (translateValue $ parse "\\x -> get x x")
  trigger <- buildNewValue triggerValue
  rightApply trigger ZeroCard
    where waste 0 value = value
          waste n value = ValueApplication (ValueApplication (ValueCard SCard) (waste (n-1) value))
                                           (ValueCard IdentityCard)

getGameState :: TestCaseGenerator GameState
getGameState = do
  (_, _, _, gs) <- get
  return gs

runMoveWriter :: MoveWriter () -> TestCaseGenerator ()
runMoveWriter moveWriter = do
  gs <- getGameState
  case execMoveWriter gs moveWriter of
    Nothing -> assert "runMoveWriter failed" (const False)
    Just moves -> makeMoves moves

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
 ("attack_11.9999", do buildNewValue (parse "attack 1 1 9999")
                       return ()),
 ("attack_11.10000", do buildNewValue (parse "attack 1 1 10000")
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
 ("help_11.9999", do buildNewValue (parse "help 1 1 9999")
                     return ()),
 ("help_11.10000", do buildNewValue (parse "help 1 1 10000")
                      return ()),
 ("help_11.10001", do buildNewValue (parse "help 1 1 10001")
                      return ()),
 ("help_dead", do buildNewValueAt (parse "S") 234
                  buildNewValue (parse "help 234 0 10000")
                  buildNewValue (parse "help 1 234 1")
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
 ("copy_dead", do buildNewValueAt (parse "S") 100
                  switchPlayers
                  buildNewValue (parse "attack 0 155 9000")
                  buildNewValue (parse "attack 1 155 9000")
                  who <- getProponent
                  assert "opponent slot 100 dead" (\gs -> gsGetVitality (perspectiveFor (opponent who) False) gs 100 == 0)
                  buildNewValueAt (parse "copy 100") 245
                  assert "slot copied" (\gs -> gsGetField (perspectiveFor who False) gs 245 == parse "S")
                  return ()),
 ("copy_non_identity", do buildNewValueAt (parse "K") 0
                          switchPlayers
                          buildNewValue (parse "copy 0")
                          assertProponent "slot copied" (\pers gs -> gsGetField pers gs 0 == parse "K")
                          return ()),
 ("lazy", do loc <- buildNewValue (parse "lazy (inc 0)")
             rightApply loc ZombieCard),
 ("health_bomb", do loc <- buildNewValue (parse "\\x -> get x (lazy (help 0 0 8196) x)")
                    rightApply loc ZeroCard),
 ("get_from_dead", do buildNewValueAt (parse "S") 200
                      buildNewValue (parse "help 200 0 10000")
                      buildNewValue (parse "get 200")
                      return ()),
 -- ZOMBIE TESTS
 ("ZKK", do buildNewValue (parse "zombie K K")
            return ()),
 ("Z0K", do buildNewValue (parse "zombie 0 K")
            return ()),
 ("ZK0", do buildNewValue (parse "zombie K 0")
            return ()),
 ("Z00", do buildNewValue (parse "zombie 0 0")
            return ()),
 ("Z.dead", do buildNewValue (parse "attack 0 123 8000")
               buildNewValue (parse "attack 1 123 8000")
               buildNewValue (parse "zombie 123 I")
               buildNewValue (parse "S")
               return ()),
 ("Z.something", do buildNewValue (parse "attack 0 123 8000")
                    buildNewValue (parse "attack 1 123 8000")
                    buildNewValue (parse "zombie 123 (S S K S S K)")
                    buildNewValue (parse "S")
                    return ()),
 ("Z.inc", do buildNewValue (parse "attack 0 123 8000")
              buildNewValue (parse "attack 1 123 8000")
              buildNewValue (parse "zombie 123 (S(K(inc))(K(5)))")
              buildNewValue (parse "S S")
              return ()),
 ("Z.dec", do buildNewValue (parse "attack 0 123 8000")
              buildNewValue (parse "attack 1 123 8000")
              buildNewValue (parse "zombie 123 (S(K(dec))(K(5)))")
              buildNewValue (parse "S S")
              return ()),
 ("Z.attack", do buildNewValue (parse "attack 0 123 8000")
                 buildNewValue (parse "attack 1 123 8000")
                 buildNewValue (parse "zombie 123 (S(K(attack 34 56))(K(1234)))")
                 buildNewValue (parse "S S")
                 return ()),
 ("Z.help", do buildNewValue (parse "attack 0 123 8000")
               buildNewValue (parse "attack 1 123 8000")
               buildNewValue (parse "zombie 123 (S(K(help 99 222))(K(4321)))")
               buildNewValue (parse "S S")
               return ()),
 -- END ZOMBIE TESTS
 ("grapeshot", do buildNewValueAt (grapeshot 8192 0) 0
                  rightApply 0 ZeroCard
                  assertProponent "collateral damage" (\pers gs -> all (\i -> gsGetVitality pers gs i == 1808) [0..65])
                  assertOpponent "inflicted damage" (\pers gs -> all (\i -> gsGetVitality pers gs i == 2628) [190..255])),
 ("firingSquad", do buildNewValueAt (firingSquad 256 100 0) 0
                    rightApply 0 ZeroCard
                    assertProponent "collateral damage" (\pers gs -> all (\i -> gsGetVitality pers gs i == 9744) [0..65])
                    assertOpponent "inflicted damage" (\pers gs -> gsGetVitality pers gs 155 == 0 )),
 ("heal", do buildNewValueAt (heal 52 8192 0) 0
             rightApply 0 ZeroCard
             assertProponent "slot healed" (\pers gs -> gsGetVitality pers gs 52 == 65535 )
             ),
 ("spreadLove", do --buildNewValueAt (heal 0 8192 0) 0
                   --rightApply 0 ZeroCard
                   -- buildNewValueAt (spreadLove 49152 0) 0
                   buildNewValueAt (spreadLove 900 0) 0
                   rightApply 0 ZeroCard
                   assertProponent "collateral damage" (\pers gs -> gsGetVitality pers gs  0 ==  9100 )
                   assertProponent "small heals" (\pers gs -> all (\i -> gsGetVitality pers gs i == 10090) [1..65])
                   assertProponent "large heal" (\pers gs -> gsGetVitality pers gs 66 == 10990 )
                   ),
 ("cureLightWounds", do --buildNewValueAt (heal 0 8192 0) 0
                     --rightApply 0 ZeroCard
                     -- buildNewValueAt (spreadLove 49152 0) 0
                     buildNewValueAt (cureLightWounds 999 0) 0
                     rightApply 0 ZeroCard
                     assertProponent "heals" (\pers gs -> all (\i -> gsGetVitality pers gs i == 10099) [0..65])
                     ),
 ("fastKill", do buildNewValueAt (fastKill 1 2 1) 0
                 return ()
                 -- assertProponent (\pers gs -> all (\i -> gsGetVitality pers gs i == 10099) [0..65])
                 ),
 ("massRaiseDead", do buildNewValueAt (fastKill 1 2 154) 0
                      buildNewValueAt (fastKill 3 4 153) 10
                      buildNewValueAt (fastKill 5 6 152) 20
                      switchPlayers
                      buildNewValueAt (massRaiseDead 0) 0
                      rightApply 0 ZeroCard
                      return ()
                      ),
 ("massResurrection", do buildNewValueAt (fastKill 1 2 244) 0
                         buildNewValueAt (fastKill 3 4 243) 10
                         buildNewValueAt (fastKill 5 6 242) 20
                         switchPlayers
                         buildNewValueAt (massResurrection 8192 0) 0
                         rightApply 0 ZeroCard
                         return ()
                         ),
 ("optimized_slot_killer", do
    buildNewValueAt (ValueNum 4) 0  -- slot[0] = 4
    buildNewValueAt (parse "get 0") 129 -- slot[2] = slot[0]  (4)
    leftApply 0 DoubleCard            -- slot[0] *= 2  (8)
    buildNewValueAt (parse "get 0") 128 -- slot[3] = slot[0] (8)
    sequence_ (replicate 9 (leftApply 0 DoubleCard)) -- slot[0] *= 2^9 (4096)
    leftApply 128 AttackCard            -- slot[2] = attack slot[2] 0 (get 1)  (executes attack 32 0 4096)
    rightApply 128 ZeroCard
    rightVineBuild 128 (parse "get 0")
    leftApply 0 DoubleCard            -- slot[0] *= 2 (8192)
    leftApply 129 AttackCard            -- slot[3] = attack slot[3] 0 (get 0)  (executes attack 64 0 8192)
    rightApply 129 ZeroCard
    rightVineBuild 129 (parse "get 0")
    assertOpponent "Opponent slot 255 killed" (\pers gs -> gsGetVitality pers gs 255 == 0 )
    return () ),
  ("zombie_lone_gunman", do
    runMoveWriter KillerOf255.speedKillTheMadBomberCell
    assertOpponent "Opponent slot 255 killed" (\pers gs -> gsGetVitality pers gs 255 == 0 )
    buildNewValueAt (goblinSapperBomb 8192 1) 1  -- I have an 8192 on cell 0. Perhpas hand-construct a bomb with copy 0 instead of damage #
    buildNewValueAt (loneZombie 0 1 0) 130
    return () ),
  ("zombie_sapper_to_low_registers", do
    runMoveWriter KillerOf255.speedKillTheMadBomberCell
    assertOpponent "Opponent slot 255 killed" (\pers gs -> gsGetVitality pers gs 255 == 0 )
    -- runMoveWriter KillerOf255.goblinSappersAtLowEnd
    return () ),
 ("killerOf255", do
    runMoveWriter KillerOf255.speedKillTheMadBomberCell
    assertOpponent "Opponent slot 255 killed" (\pers gs -> gsGetVitality pers gs 255 == 0 ))
 ]

testCaseAtomsToMoves :: String -> [TestCaseAtom] -> [Move]
testCaseAtomsToMoves testName = testCaseAtomsToMoves'
    where testCaseAtomsToMoves' [] = [nullMove, nullMove]
          testCaseAtomsToMoves' all@(TestCaseMove move : rest) = move : testCaseAtomsToMoves' rest
          testCaseAtomsToMoves' (TestCaseAssertionFailure gs msg : _)
              = error $ unlines $
                ["Assertion failure in test " ++ testName
                ,"Game state is: " ++ show (playerToMove gs) ++ " to move"
                ,"First player board:\n" ++ show (firstPlayerBoard gs)
                ,"Second player board:\n" ++ show (secondPlayerBoard gs)
                ,"Assertion message: " ++ msg
                ]

nullMove = Move LeftApplication IdentityCard 0

outputTestCase :: String -> TestCaseGenerator () -> IO ()
outputTestCase testName testCase = do
  [directory] <- getArgs
  let testCaseAtoms = execWriter (evalStateT testCase ([0..255], [0..255], FirstPlayer, initialState))
      fileContents = printMoves $ testCaseAtomsToMoves testName testCaseAtoms
  writeFile (directory ++ "/" ++ testName) fileContents

main :: IO ()
main = do
  putStrLn "Generating test cases..."
  sequence_ $ do (testName, testCase) <- testCases
                 return (outputTestCase testName testCase)
  putStrLn "Done."
