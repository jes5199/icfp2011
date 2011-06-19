module MoveStep where -- export everything :)

import Test.HUnit (Test,(~?=))
import Data.Array
import Control.Monad.State
import Control.Monad.Error
import GameState
import Value
import Move

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
                   1001 -> throwError "AppLimitExceeded"
                   _ -> putAppCount c'

getGameState :: MoveStep GameState
getGameState = get >>= (return . fst)

putGameState :: GameState -> MoveStep ()
putGameState s = do (_,c) <- get
                    put (s,c)

getProponentSlots :: MoveStep Slots
getProponentSlots = do GameState who p1 p2 _ <- getGameState
                       case who of
                         FirstPlayer  -> return p1
                         SecondPlayer -> return p2

transformProponentSlots :: (Slots -> Slots) -> MoveStep ()
transformProponentSlots transform
    = do GameState who p1 p2 z <- getGameState
         case who of
           FirstPlayer  -> putGameState (GameState who (transform p1) p2 z)
           SecondPlayer -> putGameState (GameState who p1 (transform p2) z)

getProponentSlot :: SlotNumber -> MoveStep Slot
getProponentSlot n = do Slots slots <- getProponentSlots
                        return $ slots ! n

getProponentField :: SlotNumber -> MoveStep Value
getProponentField n = do s <- getProponentSlot n
                         return $ field s

getProponentVitality :: SlotNumber -> MoveStep Vitality
getProponentVitality n = do s <- getProponentSlot n
                            return $ vitality s

putProponentField :: Value -> SlotNumber -> MoveStep ()
putProponentField v n = transformProponentSlots (updateField v n)

putProponentVitality :: Vitality -> SlotNumber -> MoveStep ()
putProponentVitality v n = transformProponentSlots (updateVitality v n)



getOpponentSlots :: MoveStep Slots
getOpponentSlots = do GameState who p1 p2 _ <- getGameState
                      case who of
                        FirstPlayer  -> return p2
                        SecondPlayer -> return p1

transformOpponentSlots :: (Slots -> Slots) -> MoveStep ()
transformOpponentSlots transform
    = do GameState who p1 p2 z <- getGameState
         case who of
           SecondPlayer  -> putGameState (GameState who (transform p1) p2 z)
           FirstPlayer   -> putGameState (GameState who p1 (transform p2) z)

getOpponentSlot :: SlotNumber -> MoveStep Slot
getOpponentSlot n = do Slots slots <- getOpponentSlots
                       return $ slots ! n

getOpponentField :: SlotNumber -> MoveStep Value
getOpponentField n = do s <- getOpponentSlot n
                        return $ field s

getOpponentVitality :: SlotNumber -> MoveStep Vitality
getOpponentVitality n = do s <- getOpponentSlot n
                           return $ vitality s

putOpponentField :: Value -> SlotNumber -> MoveStep ()
putOpponentField v n = transformOpponentSlots (updateField v n)

putOpponentVitality :: Vitality -> SlotNumber -> MoveStep ()
putOpponentVitality v n = transformOpponentSlots (updateVitality v n)

data Perspective = Perspective
                   { getVitality :: SlotNumber -> MoveStep Vitality,
                     getField :: SlotNumber -> MoveStep Value,
                     payVitalityCost :: SlotNumber -> Vitality -> MoveStep (),
                     applyVitalityConsequence :: SlotNumber -> Vitality -> MoveStep (),
                     setField :: SlotNumber -> Value -> MoveStep (),
                     setVitalityOnDeadSlot :: SlotNumber -> Vitality ->
                                              MoveStep () }

liftPerspective :: GSPerspective -> Perspective
liftPerspective p =
  Perspective
  ( \n -> do state <- getGameState
             return $ gsGetVitality p state n)
  ( \n -> do state <- getGameState
             return $ gsGetField p state n)
  ( \n v -> do state <- getGameState
               putGameState $ gsPayVitalityCost p state n v)
  ( \n v -> do state <- getGameState
               putGameState $ gsApplyVitalityConsequence p state n v)
  ( \n a -> do state <- getGameState
               putGameState $ gsSetField p state n a)
  ( \n v -> do state <- getGameState
               putGameState $ gsSetVitalityOnDeadSlot p state n v)

myFriend :: MoveStep Perspective
myFriend = do (state,_) <- get
              return $ liftPerspective $ gsMyFriend state

myEnemy :: MoveStep Perspective
myEnemy = do (state,_) <- get
             return $ liftPerspective $ gsMyEnemy state

-- Executes the lambda function corresponding to a move, incorporates
-- side effects into the GameState, and stops execution if an error
runMove step state = (newState,result)
  where
    (result,(newState,appsUsed)) = runState (runErrorT step) (state,0)

test_MoveStep = [
  ] :: [Test]
