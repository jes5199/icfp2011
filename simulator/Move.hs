module Move (Application(..),Move(..),SlotNumber,printMoves,slotNumOfMove) where

import Card

data Application = LeftApplication | RightApplication
                 deriving (Eq, Show)

type SlotNumber = Int

data Move = Move Application Card SlotNumber
          deriving (Eq, Show)

printMoves :: [Move] -> String
printMoves ms = unlines $ foldr (++) [] $ map printMove ms
    where printMove (Move LeftApplication card slotNum) = ["1", show card, show slotNum]
          printMove (Move RightApplication card slotNum) = ["2", show slotNum, show card]

slotNumOfMove :: Move -> SlotNumber
slotNumOfMove (Move _ _ slot) = slot
