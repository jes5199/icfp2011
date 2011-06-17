module Move (Application(..),Move(..)) where

import Card

data Application = LeftApplication | RightApplication
                 deriving (Eq, Show)

type SlotNumber = Int

data Move = Move Application Card SlotNumber
          deriving (Eq, Show)
