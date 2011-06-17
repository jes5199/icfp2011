module GameState (GameState(..),initialState,updateVitality,updateField,test_GameState) where

import Test.HUnit
import Data.Array
import Card
import Value

data Slot = Slot { vitality :: Int, field :: Value }
          deriving (Eq, Show)

replaceVitality hp slot = Slot hp (field slot)
replaceField value slot = Slot (vitality slot) value

type Slots = Array Int Slot

data GameState = GameState { firstPlayerBoard :: Slots, secondPlayerBoard :: Slots }
               deriving (Eq, Show)

transformSlot transformation idx slots = slots // [(idx, transformation (slots ! idx) )]

updateVitality hp idx slots = transformSlot (replaceVitality hp) idx slots
updateField value idx slots = transformSlot (replaceField value) idx slots

initialState :: GameState
initialState = GameState a a
  where
    a = array (0,255::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                            n <- [0..255]]

test_GameState = [
    updateVitality 19 1 testSlots ~?= testSlots
    ]
    where
        testSlots = array (0,3::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                            n <- [0..10]]
        idValue = cardToValue IdentityCard
