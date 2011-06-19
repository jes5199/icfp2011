module Main where

import Card
import Move
import GameState
import Simulator

main :: IO ()
main = do putStrLn "Lambda: The Gathering version $Date:: 2011-06-17 18:39:42 +0900#$"
          putStrLn "     __//////\\   Emulated by AMB,PUB,BWJ,MJQR,JAW   /\\\\\\\\\\\\__"
          playTurn initialState 1


playTurn :: GameState -> Int -> IO ()
playTurn state turnNumber = do putStrLn ("###### turn " ++ show turnNumber)
                               state1 <- playPly 0 state
                               let state2 = switchPlayer state1
                               state3 <- playPly 1 state2
                               let state4 = switchPlayer state3
                               playTurn state4 (turnNumber+1)

playPly :: Int -> GameState -> IO GameState
playPly playerNumber state =
  do putStrLn ("*** player " ++ show playerNumber ++ "'s turn, with slots:")
     putStr $ show $ case playerNumber of
       0 -> firstPlayerBoard state
       1 -> secondPlayerBoard state
     putStrLn "(slots {10000,I} are omitted)"
     let (state', zmsgs) = simulateZombies state
     mapM_ putStrLn zmsgs
     putStrLn "(1) apply card to slot, or (2) apply slot to card?"
     applicationDir <- readInt
     move <- case applicationDir of
       1 -> do card <- askCard
               slotNumber <- askSlot
               putStrLn $ "player " ++ show playerNumber ++ " applied card " ++ show card ++ " to slot " ++ show slotNumber
               return $ Move LeftApplication card slotNumber
       2 -> do slotNumber <- askSlot
               card <- askCard
               putStrLn $ "player " ++ show playerNumber ++ " applied slot " ++ show slotNumber ++ " to card " ++ show card
               return $ Move RightApplication card slotNumber
     let (game, err) = simulateTurn state' move
     either (putStrLn . (\e -> "Exception: " ++ e ++ "\nslot " ++ (show $ slotNumOfMove move) ++ " reset to I" ))
            (const $ return ()) $ err
     return game

readInt :: IO Int
readInt = do str <- getLine
             return $ read str

askCard :: IO Card
askCard = do putStrLn "card name?"
             str <- getLine
             return $ readCard str

askSlot :: IO Int
askSlot = do putStrLn "slot no?"
             readInt
