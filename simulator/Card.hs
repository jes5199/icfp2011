module Card (Card(..),Function(..),cardToFunction,identityCard) where

data Card = Card String
          deriving (Eq, Show)

data Function = IdentityFunction
              deriving (Eq, Show)

cardToFunction :: Card -> Function
cardToFunction _ = IdentityFunction

identityCard = Card "I"
