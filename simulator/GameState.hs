module GameState (GameState(..),Slots(..),Slot(..),initialState,Who(..)) where

import Data.Array
import Card
import Value

data Slot = Slot { vitality :: Int, field :: Value }
          deriving (Eq, Show)

newtype Slots = Slots (Array Int Slot)
    deriving Eq

instance Show Slots where
    show (Slots arr) = show $ filter isInteresting $ assocs arr
        where isInteresting (_, Slot 10000 (ValueCard IdentityCard)) = False
              isInteresting _ = True

data Who = FirstPlayer | SecondPlayer
         deriving (Eq, Show)

data GameState = GameState Who Slots Slots
               deriving (Eq, Show)

initialState :: GameState
initialState = GameState FirstPlayer a a
  where
    a = Slots $
        array (0,255::Int) [(n,Slot 10000 (cardToValue IdentityCard)) |
                            n <- [0..255]]
