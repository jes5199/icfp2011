module Move (Application(..),Move(..),Who(..)) where

import Card

data Who = FirstPlayer | SecondPlayer
         deriving (Eq, Show)

data Application = LeftApplication | RightApplication
                 deriving (Eq, Show)

data Move = Move Application Card Int
          deriving (Eq, Show)
