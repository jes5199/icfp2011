module Card (Card(..),readCard,test_Card) where

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
          deriving (Eq, Ord)

instance Show Card where
  show IdentityCard = "I"
  show ZeroCard = "zero"
  show SuccCard = "succ"
  show DoubleCard = "dbl"
  show GetCard = "get"
  show PutCard = "put"
  show SCard = "S"
  show KCard = "K"
  show IncCard = "inc"
  show DecCard = "dec"
  show AttackCard = "attack"
  show HelpCard = "help"
  show CopyCard = "copy"
  show ReviveCard = "revive"
  show ZombieCard = "zombie"

readCard :: String -> Card
readCard "I" = IdentityCard
readCard "zero" = ZeroCard
readCard "succ" = SuccCard
readCard "dbl" = DoubleCard
readCard "get" = GetCard
readCard "put" = PutCard
readCard "S" = SCard
readCard "K" = KCard
readCard "inc" = IncCard
readCard "dec" = DecCard
readCard "attack" = AttackCard
readCard "help" = HelpCard
readCard "copy" = CopyCard
readCard "revive" = ReviveCard
readCard "zombie" = ZombieCard
readCard _ = error "BAD CARD NO COOKIE"

test_Card = [
  ] :: [Test]
