module GameState (GameState(..),Slots(..),Slot(..),initialState,Who(..),updateVitality,updateField,test_GameState,Vitality,initialSide,switchPlayer,alterFirstBoard,opponent,gsMyFriend,gsMyEnemy,gsDamage,gsHeal,GSPerspective(..),beginZombieApocolypse,quellZombieApocolypse) where

import Test.HUnit
import Data.Array
import Card
import Value
import Move

type Vitality = Int

data Slot = Slot { vitality :: Vitality, field :: Value }
          deriving (Eq)

instance Show Slot where
    show (Slot v f) = "{" ++ (show v) ++ "," ++ (show f) ++ "}"

replaceVitality :: Vitality -> Slot -> Slot
replaceVitality hp slot = Slot hp (field slot)

addIfAlive current adj = if current <= 0 then current else current + adj

changeVitality :: Vitality -> Slot -> Slot
changeVitality hp slot = Slot (clamp (addIfAlive (vitality slot) hp)) (field slot)

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
data GSPerspective = GSPerspective
                     { gsGetVitality :: (GameState -> SlotNumber -> Vitality),
                       gsGetField :: (GameState -> SlotNumber -> Value),
                       gsModifyVitality :: (GameState -> SlotNumber ->
                                          Vitality -> GameState),
                       gsSetField :: (GameState -> SlotNumber ->
                                    Value -> GameState),
                       gsSetVitalityOnDeadSlot :: (GameState -> SlotNumber ->
                                                 Vitality -> GameState),
                       viewer :: Who,
                       zombieApocolypse :: Bool }

instance Eq GSPerspective where
    (==) lhs rhs = (viewer lhs) == (viewer rhs) && (zombieApocolypse lhs) == (zombieApocolypse rhs) 

instance Show GSPerspective where
    show (GSPerspective _ _ _ _ _ who zombies) = if zombies then "zombies are swarming " ++ (show who) else (show who) ++ " is acting"

makeGSPerspective board replaceBoard who = GSPerspective (extractVitality . board) (extractField . board)
    (\game -> \idx -> \adjustment -> replaceBoard game (changeVitalityInSlot (board game) idx adjustment))
    (\game -> \idx -> \value -> replaceBoard game (updateField value idx (board game)))
    (\game -> \idx -> \vitality -> replaceBoard game (replaceVitalityOnDeadSlot (board game) idx vitality))
    who

firstPersonView = makeGSPerspective firstPlayerBoard
    (\game -> \newSlots -> GameState (playerToMove game) newSlots (secondPlayerBoard game) (zombiesAreOut game)) FirstPlayer
secondPersonView = makeGSPerspective secondPlayerBoard
    (\game -> \newSlots -> GameState (playerToMove game) (firstPlayerBoard game) newSlots (zombiesAreOut game)) SecondPlayer

-- The perspective of a particular player.
perspectiveFor :: Who -> (Bool-> GSPerspective)
perspectiveFor who =
    case who of
        FirstPlayer  -> firstPersonView
        SecondPlayer -> secondPersonView

-- The current actor's friend. An actor is a player or a zombie master.
gsMyFriend :: GameState -> GSPerspective
gsMyFriend (GameState who p1 p2 zombies) =
    perspectiveFor who zombies

-- The current actor's enemy. An actor is a player or a zombie master.
gsMyEnemy :: GameState -> GSPerspective
gsMyEnemy (GameState who p1 p2 zombies) =
    perspectiveFor (opponent who) zombies

gsDamage :: Int -> GameState -> Int
gsDamage val (GameState _ _ _ zombies) =
    if zombies then val else (- val)

gsHeal :: Int -> GameState -> Int
gsHeal val (GameState _ _ _ zombies) =
    if zombies then (- val) else val

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
              array (0,255::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                                  n <- [0..255]]

test_GameState = [
    updateVitality 19 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 19 idValue)]),
    updateField (ValueNum 5) 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 10000 (ValueNum 5))]),

    gsMyFriend startingGame ~?= player1,
    gsMyFriend (switchPlayer startingGame) ~?= player2,
    gsMyFriend zombieTime ~?= zombie1,
    gsMyFriend (switchPlayer zombieTime) ~?= zombie2,
    gsMyEnemy startingGame ~?= player2,
    gsMyEnemy (switchPlayer startingGame) ~?= player1,
    gsMyEnemy zombieTime ~?= zombie2,
    gsMyEnemy (switchPlayer zombieTime) ~?= zombie1,

    gsGetVitality player1 startingGame 3 ~?= 8000,
    gsGetVitality player2 startingGame 3 ~?= 12000,
    gsGetField player1 startingGame 0 ~?= valueHelp,
    gsGetField player2 startingGame 0 ~?= valueAttack,
    gsGetVitality player1 (gsModifyVitality player1 startingGame 3 (-100)) 3 ~?= 7900,
    gsGetVitality player2 (gsModifyVitality player2 startingGame 3 100) 3 ~?= 12100,
    gsGetField player1 (gsSetField player1 startingGame 0 valueZombie) 0 ~?= valueZombie,
    gsGetField player2 (gsSetField player2 startingGame 0 valueRevive) 0 ~?= valueRevive,

    gsGetVitality player1 (gsModifyVitality player1 startingGame 3 (-10000)) 3 ~?= 0,
    gsGetVitality player2 (gsModifyVitality player2 startingGame 3 70000) 3 ~?= 65535,

    gsGetVitality player1 (gsSetVitalityOnDeadSlot player1 someDeath 3 1) 3 ~?= 1,
    gsGetVitality player2 (gsSetVitalityOnDeadSlot player2 someDeath 3 (-1)) 3 ~?= (-1),

    gsGetVitality player1 (gsModifyVitality player1 someDeath 3 5000) 3 ~?= 0,

    gsDamage 33 startingGame ~?= -33,
    gsDamage 66 zombieTime ~?= 66,

    gsHeal 33 startingGame ~?= 33,
    gsHeal 66 zombieTime ~?= -66
    ]
  where
    firstSide = updateField valueHelp 0 (updateVitality 8000 3 initialSide)
    secondSide = updateField valueAttack 0 (updateVitality 12000 3 initialSide)
    deadCellSide = updateVitality 0 3 initialSide
    startingGame = GameState FirstPlayer firstSide secondSide False
    zombieTime = GameState FirstPlayer firstSide secondSide True
    someDeath = GameState FirstPlayer deadCellSide deadCellSide True
    testSlots = array (0,3::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                        n <- [0..3]]
    idValue = cardToValue IdentityCard
    player1 = firstPersonView False
    player2 = secondPersonView False
    zombie1 = firstPersonView True
    zombie2 = secondPersonView True
