module Move (Application(..),Move(..)) where

import Card

data Application = LeftApplication | RightApplication
                 deriving (Eq, Show)

data Move = Move Application Card Int
          deriving (Eq, Show)
