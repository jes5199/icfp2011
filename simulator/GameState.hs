module GameState (GameState(..),Slots(..),Slot(..),initialState,Who(..),updateVitality,updateField,test_GameState,Vitality,initialSide,switchPlayer,alterFirstBoard,opponent,getFriend,myEnemy) where

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

replaceField :: Value -> Slot -> Slot
replaceField value slot = Slot (vitality slot) value

newtype Slots = Slots (Array Int Slot)
    deriving Eq

instance Show Slots where
    show (Slots arr) = concatMap showSlotOnALine $ filter isInteresting $ assocs arr
        where isInteresting (_, Slot 10000 (ValueCard IdentityCard)) = False
              isInteresting _ = True
              showSlotOnALine (n, slot) = (show n) ++ "=" ++ (show slot) ++ "\n"

transformSlot :: (Slot -> Slot) -> Int -> Slots -> Slots
transformSlot transformation idx (Slots slots) = Slots $ slots // [(idx, transformation (slots ! idx) )]

updateVitality :: Vitality -> SlotNumber -> Slots -> Slots
updateVitality hp idx slots = transformSlot (replaceVitality hp) idx slots

updateField :: Value -> SlotNumber -> Slots -> Slots
updateField value idx slots = transformSlot (replaceField value) idx slots

data Who = FirstPlayer | SecondPlayer
         deriving (Eq, Show)

opponent FirstPlayer = SecondPlayer
opponent SecondPlayer = FirstPlayer

data GameState = GameState { playerToMove :: Who, firstPlayerBoard :: Slots, secondPlayerBoard :: Slots, zombiesAreOut :: Bool }
               deriving (Eq, Show)

-- This is a perspective on the board, as viewed by some player or zombie.
data Perspective = Perspective { getVitality :: (SlotNumber -> GameState -> Vitality), getField :: (SlotNumber -> GameState -> Value),
    modifyVitality :: (Vitality -> SlotNumber -> GameState), setField :: (Value -> SlotNumber -> GameState), viewer :: Who }

instance Eq Perspective where
    (==) lhs rhs = (viewer lhs) == (viewer rhs)

instance Show Perspective where
    show (Perspective _ _ _ _ who) = (show who) ++ " point of view"

firstPersonView = Perspective undefined undefined undefined undefined FirstPlayer
secondPersonView = Perspective undefined undefined undefined undefined SecondPlayer

--attack i j n state =
--  adjustVitality (myEnemy state) 19 (damage state 33)

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

    damage 33 startingGame ~?= -33,
    damage 66 zombieTime ~?= 66,

    heal 33 startingGame ~?= 33,
    heal 66 zombieTime ~?= -66
    ]
  where
    firstSide = updateField valueHelp 0 initialSide
    secondSide = updateField valueAttack 0 initialSide
    startingGame = GameState FirstPlayer firstSide secondSide False
    zombieTime = GameState FirstPlayer firstSide secondSide True
    testSlots = array (0,3::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                        n <- [0..3]]
    idValue = cardToValue IdentityCard
