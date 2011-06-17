module Move (Application(..),Move(..),SlotNumber) where

import Card

data Application = LeftApplication | RightApplication
                 deriving (Eq, Show)

type SlotNumber = Int

data Move = Move Application Card SlotNumber
          deriving (Eq, Show)
