module GameState (GameState(..),Slots(..),Slot(..),initialState,Who(..),updateVitality,updateField,test_GameState,Vitality,initialSide,switchPlayer,alterFirstBoard,opponent) where

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

newtype Slots = Slots (Array Int Slot)
    deriving Eq

instance Show Slots where
    show (Slots arr) = concatMap showSlotOnALine $ filter isInteresting $ assocs arr
        where isInteresting (_, Slot 10000 (ValueCard IdentityCard)) = False
              isInteresting _ = True
              showSlotOnALine (n, slot) = (show n) ++ "=" ++ (show slot) ++ "\n"

replaceVitality :: Vitality -> Slot -> Slot
replaceVitality hp slot = Slot hp (field slot)

replaceField :: Value -> Slot -> Slot
replaceField value slot = Slot (vitality slot) value

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
    updateField (ValueNum 5) 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 10000 (ValueNum 5))])
    ]
    where
        testSlots = array (0,3::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                            n <- [0..3]]
        idValue = cardToValue IdentityCard
