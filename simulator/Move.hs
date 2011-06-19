module Move (Application(..),Move(..),printMoves,printMove,slotNumOfMove) where

import Card
import Slots

data Application = LeftApplication | RightApplication
                 deriving (Eq, Show, Ord)

data Move = Move Application Card SlotNumber
          deriving (Eq, Show, Ord)

printMoves :: [Move] -> String
printMoves ms = unlines $ foldr (++) [] $ map printMove ms

printMove (Move LeftApplication card slotNum) = ["1", show card, show slotNum]
printMove (Move RightApplication card slotNum) = ["2", show slotNum, show card]

slotNumOfMove :: Move -> SlotNumber
slotNumOfMove (Move _ _ slot) = slot
