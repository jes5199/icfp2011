module GameState (GameState(..),Slots(..),Slot(..),initialState,Who(..),updateVitality,updateField,test_GameState,Vitality,initialSide) where

import Test.HUnit
import Data.Array
import Card
import Value
import Move

type Vitality = Int

data Slot = Slot { vitality :: Vitality, field :: Value }
          deriving (Eq, Show)

newtype Slots = Slots (Array Int Slot)
    deriving Eq

instance Show Slots where
    show (Slots arr) = show $ filter isInteresting $ assocs arr
        where isInteresting (_, Slot 10000 (ValueCard IdentityCard)) = False
              isInteresting _ = True

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

data GameState = GameState { playerToMove :: Who, firstPlayerBoard :: Slots, secondPlayerBoard :: Slots }
               deriving (Eq, Show)

initialState :: GameState
initialState = GameState FirstPlayer initialSide initialSide

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
