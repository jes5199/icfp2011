module MoveStep where -- export everything :)

import Control.Monad.State
import Control.Monad.Error
import GameState

type MoveStep = ErrorT String (State (GameState,Int))

getGameState :: MoveStep GameState
getGameState = get >>= (return . fst)

putGameState :: GameState -> MoveStep ()
putGameState s = do (_,c) <- get
                    put (s,c)

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

-- Executes the lambda function corresponding to a move, incorporates
-- side effects into the GameState, and stops execution if an error
-- occurs.
runMove :: MoveStep a -> GameState -> (GameState,Either String a)
runMove step state = (newState,result)
  where
    (result,(newState,appsUsed)) = runState (runErrorT step) (state,0)
