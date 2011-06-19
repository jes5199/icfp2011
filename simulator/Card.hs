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
  readCard (show IdentityCard) ~?= IdentityCard,
  readCard (show ZeroCard) ~?= ZeroCard,
  readCard (show SuccCard) ~?= SuccCard,
  readCard (show DoubleCard) ~?= DoubleCard,
  readCard (show GetCard) ~?= GetCard,
  readCard (show PutCard) ~?= PutCard,
  readCard (show SCard) ~?= SCard,
  readCard (show KCard) ~?= KCard,
  readCard (show IncCard) ~?= IncCard,
  readCard (show DecCard) ~?= DecCard,
  readCard (show AttackCard) ~?= AttackCard,
  readCard (show HelpCard) ~?= HelpCard,
  readCard (show CopyCard) ~?= CopyCard,
  readCard (show ReviveCard) ~?= ReviveCard,
  readCard (show ZombieCard) ~?= ZombieCard,
  show (readCard "I") ~?= "I",
  show (readCard "zero") ~?= "zero",
  show (readCard "succ") ~?= "succ",
  show (readCard "dbl") ~?= "dbl",
  show (readCard "get") ~?= "get",
  show (readCard "put") ~?= "put",
  show (readCard "S") ~?= "S",
  show (readCard "K") ~?= "K",
  show (readCard "inc") ~?= "inc",
  show (readCard "dec") ~?= "dec",
  show (readCard "attack") ~?= "attack",
  show (readCard "help") ~?= "help",
  show (readCard "copy") ~?= "copy",
  show (readCard "revive") ~?= "revive",
  show (readCard "zombie") ~?= "zombie"
  ]
