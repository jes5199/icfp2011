module MoveStep where -- export everything :)

import Data.Array
import Control.Monad.State
import Control.Monad.Error
import GameState
import Value

type MoveStep = ErrorT String (State (GameState,Int))

getAppCount :: MoveStep Int
getAppCount = get >>= (return . snd)

putAppCount :: Int -> MoveStep ()
putAppCount c = do (s,_) <- get
                   put (s,c)

incAppCount :: MoveStep ()
incAppCount = do c <- getAppCount
                 let c' = c+1
                 case c' of
                   1000 -> throwError "AppLimitExceeded"
                   _ -> putAppCount c'

getGameState :: MoveStep GameState
getGameState = get >>= (return . fst)

putGameState :: GameState -> MoveStep ()
putGameState s = do (_,c) <- get
                    put (s,c)

getProponentSlots :: MoveStep Slots
getProponentSlots = do (GameState _ p1 p2,_) <- get
                       return p1

getProponentSlot :: Int -> MoveStep Slot
getProponentSlot n = do Slots slots <- getProponentSlots
                        return $ slots ! n

getProponentSlotField :: Int -> MoveStep Value
getProponentSlotField n = do s <- getProponentSlot n
                             return $ field s

-- Executes the lambda function corresponding to a move, incorporates
-- side effects into the GameState, and stops execution if an error
-- occurs.
runMove :: MoveStep a -> GameState -> (GameState,Either String a)
runMove step state = (newState,result)
  where
    (result,(newState,appsUsed)) = runState (runErrorT step) (state,0)
