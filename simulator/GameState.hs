module GameState (GameState(..),Slots(..),Slot(..),initialState,Who(..),updateVitality,updateField,test_GameState,Vitality,initialSide,switchPlayer,alterFirstBoard,opponent,gsMyFriend,gsMyEnemy,GSPerspective,beginZombieApocolypse,quellZombieApocolypse,perspectiveFor,gsGetVitality,gsGetField,gsPayVitalityCost,gsApplyVitalityConsequence,gsSetField,gsSetVitalityOnDeadSlot) where

import Test.HUnit
import Data.Array
import Card
import Value
import Move
import Util

type Vitality = Int

data Slot = Slot { vitality :: !Vitality, field :: !Value }
          deriving (Eq)

instance Show Slot where
    show (Slot v f) = "{" ++ (show v) ++ "," ++ (show f) ++ "}"

replaceVitality :: Vitality -> Slot -> Slot
replaceVitality hp slot = Slot hp (field slot)

addIfAlive current adj = if current <= 0 then current else current + adj

changeVitality :: Vitality -> Slot -> Slot
changeVitality hp slot = Slot (clampInt 0 65535
                               (addIfAlive (vitality slot) hp)) (field slot)

replaceVitalityIfDead :: Vitality -> Slot -> Slot
replaceVitalityIfDead hp slot = if (vitality slot) > 0 then slot else Slot hp (field slot)

replaceField :: Value -> Slot -> Slot
replaceField value slot = Slot (vitality slot) value

newtype Slots = Slots (Array Int Slot)
    deriving Eq

instance Show Slots where
    show (Slots arr) = concatMap showSlotOnALine $ filter isInteresting $ assocs arr
        where isInteresting (_, Slot 10000 (ValueCard IdentityCard)) = False
              isInteresting _ = True
              showSlotOnALine (n, slot) = (show n) ++ "=" ++ (show slot) ++ "\n"

transformSlot :: (Slot -> Slot) -> SlotNumber -> Slots -> Slots
transformSlot transformation idx (Slots slots) = Slots $ slots // [(idx, transformation (slots ! idx) )]

updateVitality :: Vitality -> SlotNumber -> Slots -> Slots
updateVitality hp idx slots = transformSlot (replaceVitality hp) idx slots

changeVitalityInSlot :: Slots -> SlotNumber -> Vitality -> Slots
changeVitalityInSlot slots idx adjustment = transformSlot (changeVitality adjustment) idx slots

replaceVitalityOnDeadSlot :: Slots -> SlotNumber -> Vitality -> Slots
replaceVitalityOnDeadSlot slots idx vitality = transformSlot (replaceVitalityIfDead vitality) idx slots

updateField :: Value -> SlotNumber -> Slots -> Slots
updateField value idx slots = transformSlot (replaceField value) idx slots

extractVitality :: Slots -> SlotNumber -> Vitality
extractVitality (Slots slots) idx = vitality (slots ! idx)

extractField :: Slots -> SlotNumber -> Value
extractField (Slots slots) idx = field (slots ! idx)

data Who = FirstPlayer | SecondPlayer
         deriving (Eq, Show)

opponent FirstPlayer = SecondPlayer
opponent SecondPlayer = FirstPlayer

data GameState = GameState { playerToMove :: Who, firstPlayerBoard :: Slots, secondPlayerBoard :: Slots, zombiesAreOut :: Bool }
               deriving (Eq, Show)

-- This is a perspective on the board, as viewed by some player or zombie.
-- "player1's view of player 2's board" means actor is FirstPlayer, viewer is SecondPlayer.
data GSPerspective = GSPerspective { viewer :: Who, zombieApocolypse :: Bool }
    deriving Eq

gsGetBoard :: Who -> GameState -> Slots
gsGetBoard FirstPlayer = firstPlayerBoard
gsGetBoard SecondPlayer = secondPlayerBoard

gsGetVitality :: GSPerspective -> GameState -> SlotNumber -> Vitality
gsGetVitality pers = extractVitality . gsGetBoard (viewer pers)

gsGetField :: GSPerspective -> GameState -> SlotNumber -> Value
gsGetField pers = extractField . gsGetBoard (viewer pers)

gsSetBoard :: Who -> GameState -> Slots -> GameState
gsSetBoard FirstPlayer = replaceFirstBoard
gsSetBoard SecondPlayer = replaceSecondBoard

gsSetField :: GSPerspective -> GameState -> SlotNumber -> Value -> GameState
gsSetField pers game idx value = gsSetBoard who game (updateField value idx (gsGetBoard who game))
    where who = viewer pers

gsPayVitalityCost :: GSPerspective -> GameState -> SlotNumber -> Vitality -> GameState
gsPayVitalityCost pers game idx cost = gsSetBoard who game (changeVitalityInSlot (gsGetBoard who game) idx (-cost))
    where who = viewer pers

gsApplyVitalityConsequence :: GSPerspective -> GameState -> SlotNumber -> Vitality -> GameState
gsApplyVitalityConsequence pers game idx deltaHp
    = gsSetBoard who game (changeVitalityInSlot (gsGetBoard who game) idx (if zombieApocolypse pers then (-deltaHp) else deltaHp))
    where who = viewer pers

gsSetVitalityOnDeadSlot :: GSPerspective -> GameState -> SlotNumber -> Vitality -> GameState
gsSetVitalityOnDeadSlot pers game idx hp = gsSetBoard who game (replaceVitalityOnDeadSlot (gsGetBoard who game) idx hp)
    where who = viewer pers

instance Show GSPerspective where
    show pers = if zombieApocolypse pers
                then "zombies are swarming " ++ show (viewer pers)
                else show (viewer pers) ++ " is acting"

replaceFirstBoard (GameState who _ p2 zombies) newSlots = GameState who newSlots p2 zombies
replaceSecondBoard (GameState who p1 _ zombies) newSlots = GameState who p1 newSlots zombies

-- The perspective of a particular player.
perspectiveFor :: Who -> Bool-> GSPerspective
perspectiveFor = GSPerspective

-- The current actor's friend. An actor is a player or a zombie master.
gsMyFriend :: GameState -> GSPerspective
gsMyFriend (GameState who p1 p2 zombies) =
    perspectiveFor who zombies

-- The current actor's enemy. An actor is a player or a zombie master.
gsMyEnemy :: GameState -> GSPerspective
gsMyEnemy (GameState who p1 p2 zombies) =
    perspectiveFor (opponent who) zombies

alterFirstBoard :: (Slots -> Slots) -> GameState -> GameState
alterFirstBoard transform (GameState who firstBoard secondBoard zombies) = GameState who (transform firstBoard) secondBoard zombies

switchPlayer :: GameState -> GameState
switchPlayer (GameState FirstPlayer side1 side2 zombies) =
  GameState SecondPlayer side1 side2 zombies
switchPlayer (GameState SecondPlayer side1 side2 zombies) =
  GameState FirstPlayer side1 side2 zombies

beginZombieApocolypse (GameState who s1 s2 _) = GameState who s1 s2 True
quellZombieApocolypse (GameState who s1 s2 _) = GameState who s1 s2 False

initialState :: GameState
initialState = GameState FirstPlayer initialSide initialSide False

initialSide :: Slots
initialSide = Slots $
              array (0,255::Int) [(n,Slot 10000 (valueI)) |
                                  n <- [0..255]]

test_GameState = [
    updateVitality 19 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 19 valueI)]),
    updateField (ValueNum 5) 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 10000 (ValueNum 5))]),

    -- Begin tests to get the right perspectives
    gsMyFriend startingGame ~?= player1,
    gsMyFriend (switchPlayer startingGame) ~?= player2,
    gsMyFriend zombieTime ~?= zombie1,
    gsMyFriend (switchPlayer zombieTime) ~?= zombie2,
    gsMyEnemy startingGame ~?= player2,
    gsMyEnemy (switchPlayer startingGame) ~?= player1,
    gsMyEnemy zombieTime ~?= zombie2,
    gsMyEnemy (switchPlayer zombieTime) ~?= zombie1,
    -- End tests to get the right perspectives

    -- Begin tests of perspective behaviors
    gsGetVitality player1 startingGame 3 ~?= 8000,
    gsGetVitality player2 startingGame 3 ~?= 12000,
    gsGetField player1 startingGame 0 ~?= valueHelp,
    gsGetField player2 startingGame 0 ~?= valueAttack,

    gsGetVitality zombie1 zombieTime 3 ~?= 8000,
    gsGetVitality zombie2 zombieTime 3 ~?= 12000,
    gsGetField zombie1 zombieTime 0 ~?= valueHelp,
    gsGetField zombie2 zombieTime 0 ~?= valueAttack,

    gsGetVitality player1 (gsPayVitalityCost player1 startingGame 3 100) 3 ~?= 7900,
    gsGetVitality player2 (gsPayVitalityCost player2 startingGame 3 100) 3 ~?= 11900,
    gsGetVitality player1 (gsPayVitalityCost zombie1 zombieTime 3 100) 3 ~?= 7900,
    gsGetVitality player2 (gsPayVitalityCost zombie2 zombieTime 3 100) 3 ~?= 11900,

    gsGetVitality player1 (gsApplyVitalityConsequence player1 startingGame 3 (-100)) 3 ~?= 7900,
    gsGetVitality player2 (gsApplyVitalityConsequence player2 startingGame 3 100) 3 ~?= 12100,
    gsGetVitality player1 (gsApplyVitalityConsequence zombie1 zombieTime 3 (-100)) 3 ~?= 8100,
    gsGetVitality player2 (gsApplyVitalityConsequence zombie2 zombieTime 3 100) 3 ~?= 11900,

    gsGetField player1 (gsSetField player1 startingGame 0 valueZombie) 0 ~?= valueZombie,
    gsGetField player2 (gsSetField player2 startingGame 0 valueRevive) 0 ~?= valueRevive,

    gsGetField player1 (gsSetField zombie1 zombieTime 0 valueZombie) 0 ~?= valueZombie,
    gsGetField player2 (gsSetField zombie2 zombieTime 0 valueRevive) 0 ~?= valueRevive,

    gsGetVitality player1 (gsSetVitalityOnDeadSlot player1 someDeath 3 1) 3 ~?= 1,
    gsGetVitality player2 (gsSetVitalityOnDeadSlot player2 someDeath 3 (-1)) 3 ~?= (-1),
    gsGetVitality player1 (gsSetVitalityOnDeadSlot zombie1 someDeath 3 1) 3 ~?= 1,
    gsGetVitality player2 (gsSetVitalityOnDeadSlot zombie2 someDeath 3 (-1)) 3 ~?= (-1),
    -- End tests of perspective behaviors

    -- Begin boundary check tests
    gsGetVitality player1 (gsApplyVitalityConsequence player1 startingGame 3 (-10000)) 3 ~?= 0,
    gsGetVitality player1 (gsApplyVitalityConsequence player1 startingGame 3 70000) 3 ~?= 65535,
    gsGetVitality player1 (gsSetVitalityOnDeadSlot player1 startingGame 3 1) 3 ~?= 8000,
    gsGetVitality player1 (gsApplyVitalityConsequence player1 someDeath 3 5000) 3 ~?= 0
    -- End boundary check tests
    ]
  where
    firstSide = updateField valueHelp 0 (updateVitality 8000 3 initialSide)
    secondSide = updateField valueAttack 0 (updateVitality 12000 3 initialSide)
    deadCellSide = updateVitality 0 3 initialSide
    startingGame = GameState FirstPlayer firstSide secondSide False
    zombieTime = GameState FirstPlayer firstSide secondSide True
    someDeath = GameState FirstPlayer deadCellSide deadCellSide True
    testSlots = array (0,3::Int) [(n,Slot 10000 (valueI)) |
                        n <- [0..3]]
    player1 = perspectiveFor FirstPlayer False
    player2 = perspectiveFor SecondPlayer False
    zombie1 = perspectiveFor FirstPlayer True
    zombie2 = perspectiveFor SecondPlayer True
