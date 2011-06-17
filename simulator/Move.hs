module Move (Application(..),Move(..),printMoves) where

import Card

data Application = LeftApplication | RightApplication
                 deriving (Eq, Show)

type SlotNumber = Int

data Move = Move Application Card SlotNumber
          deriving (Eq, Show)

printMoves :: [Move] -> String
printMoves ms = unlines $ foldr (++) [] $ map printMove ms
    where printMove (Move LeftApplication card slotNum) = ["1", printCard card, show slotNum]
          printMove (Move RightApplication card slotNum) = ["2", show slotNum, printCard card]
          printCard IdentityCard = "I"
          printCard ZeroCard = "zero"
          printCard SuccCard = "succ"
          printCard DoubleCard = "dbl"
          printCard GetCard = "get"
          printCard PutCard = "put"
          printCard SCard = "S"
          printCard KCard = "K"
          printCard IncCard = "inc"
	  printCard DecCard = "dec"
	  printCard AttackCard = "attack"
	  printCard HelpCard = "help"
	  printCard CopyCard = "copy"
	  printCard ReviveCard = "revive"
	  printCard ZombieCard = "zombie"
