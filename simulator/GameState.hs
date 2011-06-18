module GameState (GameState(..),Slots(..),Slot(..),initialState,Who(..),updateVitality,updateField,test_GameState,Vitality,initialSide,switchPlayer,alterFirstBoard,opponent,myFriend,myEnemy) where

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
data Perspective = Perspective { getVitality :: (GameState -> SlotNumber -> Vitality), getField :: (GameState -> SlotNumber -> Value),
    modifyVitality :: (GameState -> SlotNumber -> Vitality -> GameState), setField :: (GameState -> SlotNumber -> Value -> GameState),
    setVitalityOnDeadSlot :: (GameState -> SlotNumber -> Vitality -> GameState), viewer :: Who }

instance Eq Perspective where
    (==) lhs rhs = (viewer lhs) == (viewer rhs)

instance Show Perspective where
    show (Perspective _ _ _ _ _ who) = (show who) ++ " point of view"

makePerspective board replaceBoard who = Perspective (extractVitality . board) (extractField . board)
    (\game -> \idx -> \adjustment -> replaceBoard game (changeVitalityInSlot (board game) idx adjustment))
    (\game -> \idx -> \value -> replaceBoard game (updateField value idx (board game)))
    (\game -> \idx -> \vitality -> replaceBoard game (replaceVitalityOnDeadSlot (board game) idx vitality))
    who

firstPersonView = makePerspective firstPlayerBoard
    (\game -> \newSlots -> GameState (playerToMove game) newSlots (secondPlayerBoard game) (zombiesAreOut game)) FirstPlayer
secondPersonView = makePerspective secondPlayerBoard
    (\game -> \newSlots -> GameState (playerToMove game) (firstPlayerBoard game) newSlots (zombiesAreOut game)) SecondPlayer

-- The perspective of a particular player.
perspectiveFor :: Who -> Perspective
perspectiveFor who =
    case who of
        FirstPlayer  -> firstPersonView
        SecondPlayer -> secondPersonView

-- The current actor's friend. An actor is a player or a zombie master.
myFriend :: GameState -> Perspective
myFriend (GameState who p1 p2 zombies) =
    if zombies then perspectiveFor (opponent who) else perspectiveFor who

-- The current actor's enemy. An actor is a player or a zombie master.
myEnemy :: GameState -> Perspective
myEnemy (GameState who p1 p2 zombies) =
    if zombies then perspectiveFor who else perspectiveFor (opponent who)

damage :: Int -> GameState -> Int
damage val (GameState _ _ _ zombies) =
    if zombies then val else (- val)

heal :: Int -> GameState -> Int
heal val (GameState _ _ _ zombies) =
    if zombies then (- val) else val

alterFirstBoard :: (Slots -> Slots) -> GameState -> GameState
alterFirstBoard transform (GameState who firstBoard secondBoard zombies) = GameState who (transform firstBoard) secondBoard zombies

switchPlayer :: GameState -> GameState
switchPlayer (GameState FirstPlayer side1 side2 zombies) =
  GameState SecondPlayer side1 side2 zombies
switchPlayer (GameState SecondPlayer side1 side2 zombies) =
  GameState FirstPlayer side1 side2 zombies

initialState :: GameState
initialState = GameState FirstPlayer initialSide initialSide False

initialSide :: Slots
initialSide = Slots $
              array (0,255::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                                  n <- [0..255]]

test_GameState = [
    updateVitality 19 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 19 idValue)]),
    updateField (ValueNum 5) 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 10000 (ValueNum 5))]),
    
    myFriend startingGame ~?= firstPersonView,
    myFriend (switchPlayer startingGame) ~?= secondPersonView,
    myFriend zombieTime ~?= secondPersonView,
    myFriend (switchPlayer zombieTime) ~?= firstPersonView,
    myEnemy startingGame ~?= secondPersonView,
    myEnemy (switchPlayer startingGame) ~?= firstPersonView,
    myEnemy zombieTime ~?= firstPersonView,
    myEnemy (switchPlayer zombieTime) ~?= secondPersonView,

    getVitality firstPersonView startingGame 3 ~?= 8000,
    getVitality secondPersonView startingGame 3 ~?= 12000,
    getField firstPersonView startingGame 0 ~?= valueHelp,
    getField secondPersonView startingGame 0 ~?= valueAttack,
    getVitality firstPersonView (modifyVitality firstPersonView startingGame 3 (-100)) 3 ~?= 7900,
    getVitality secondPersonView (modifyVitality secondPersonView startingGame 3 100) 3 ~?= 12100,
    getField firstPersonView (setField firstPersonView startingGame 0 valueZombie) 0 ~?= valueZombie,
    getField secondPersonView (setField secondPersonView startingGame 0 valueRevive) 0 ~?= valueRevive,

    getVitality firstPersonView (modifyVitality firstPersonView startingGame 3 (-10000)) 3 ~?= 0,
    getVitality secondPersonView (modifyVitality secondPersonView startingGame 3 70000) 3 ~?= 65535,

    getVitality firstPersonView (setVitalityOnDeadSlot firstPersonView someDeath 3 1) 3 ~?= 1,
    getVitality secondPersonView (setVitalityOnDeadSlot secondPersonView someDeath 3 (-1)) 3 ~?= (-1),

    getVitality firstPersonView (modifyVitality firstPersonView someDeath 3 5000) 3 ~?= 0,

    damage 33 startingGame ~?= -33,
    damage 66 zombieTime ~?= 66,

    heal 33 startingGame ~?= 33,
    heal 66 zombieTime ~?= -66
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
