module Card (Card(..),Function(..),cardToFunction) where

data Card = IdentityCard
          deriving (Eq, Show)

data Function = FunctionCard Card |
                FunctionValue Int |
                FunctionApplication Function Function
              deriving (Eq, Show)

cardToFunction :: Card -> Function
cardToFunction = FunctionCard
