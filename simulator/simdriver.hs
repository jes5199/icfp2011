module Main where

import Card
import Move
import GameState
import Simulator

main :: IO ()
main = do putStrLn "Lambda: The Gathering version $Date:: 2011-06-17 18:39:42 +0900#$"
          playTurn initialState 1

playTurn :: GameState -> Int -> IO ()
playTurn state turnNumber = do putStrLn ("###### turn " ++ show turnNumber)
                               state' <- playPly 0 state
                               state'' <- playPly 1 state'
                               playTurn state'' (turnNumber+1)

playPly :: Int -> GameState -> IO GameState
playPly playerNumber state =
  do putStrLn ("*** player " ++ show playerNumber ++ "'s turn, with slots:")
     putStrLn "... slots that difer go here"
     putStrLn "(slots {10000,I} are omitted)"
     putStrLn "(1) apply card to slot, or (2) apply slot to card?"
     applicationDir <- readInt
     case applicationDir of
       1 -> do cardName <- askCard
               slotNumber <- askSlot
               putStrLn $ "player " ++ show playerNumber ++ " applied card " ++ cardName ++ " to slot " ++ show slotNumber
               return state
       2 -> do slotNumber <- askSlot
               cardName <- askCard
               putStrLn $ "player " ++ show playerNumber ++ " applied slot " ++ show slotNumber ++ " to card " ++ cardName
               return state

readInt :: IO Int
readInt = do str <- getLine
             return $ read str

askCard :: IO String
askCard = do putStrLn "card name?"
             getLine

askSlot :: IO Int
askSlot = do putStrLn "slot no?"
             readInt

{-
let state = initialState
              move = Move LeftApplication IdentityCard 1
              result = simulate state move
          print result
-}
