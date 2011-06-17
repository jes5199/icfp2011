module Card (Card(..),test_Card) where

import Test.HUnit

data Card = IdentityCard |
            ZeroCard     |
            SuccCard     |
            DoubleCard   |
            GetCard      |
            PutCard      |
            SCard        |
            KCard        |
            IncCard      |
	    DecCard	 |
	    AttackCard   |
	    HelpCard     |
	    CopyCard     |
	    ReviveCard   |
	    ZombieCard
          deriving (Eq, Show)

test_Card = [
  ] :: [Test]
