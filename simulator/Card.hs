module Card (Card(..),test_Card) where

import Test.HUnit

data Card = IdentityCard |
            ZeroCard
          deriving (Eq, Show)

test_Card = [
  ] :: [Test]
