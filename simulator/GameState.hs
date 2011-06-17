module GameState (GameState(..),initialState) where

import Data.Array
import Card
import Function

data Slot = Slot { vitality :: Int, field :: Function }
          deriving (Eq, Show)

type Slots = Array Int Slot

data GameState = GameState Slots Slots
               deriving (Eq, Show)

initialState :: GameState
initialState = GameState a a
  where
    a = array (0,255::Int) [(n,Slot 10000 (cardToFunction IdentityCard)) |
                            n <- [0..255]]
