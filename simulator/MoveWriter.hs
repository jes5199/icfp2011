module MoveWriter where

import Control.Monad.Writer.Strict
import Control.Monad.State

import GameState
import Move
import Card
import SimpleBuilder
import Simulator
import Value
import Translator

type MoveWriter = StateT GameState (WriterT [Move] Maybe)

execMoveWriter :: GameState -> MoveWriter () -> Maybe [Move]
execMoveWriter gs moveWriter = execWriterT (evalStateT moveWriter gs)

assertConstructionCost _ = return ()
assertSlotsUsed _ = return ()

-- Note: when applying moves, we simulate our turn but not zombie
-- actions.  So we won't have a perfect prediction of future game
-- state but hopefully it will be good enough to make plans from.
move :: Move -> MoveWriter ()
move m = do
  gs <- get
  (gs', Right ()) <- return (simulateTurn gs m)
  put gs'
  tell [m]

moves :: [Move] -> MoveWriter()
moves = mapM_ move

leftApply slotNum card = move $ Move LeftApplication card slotNum
rightApply slotNum card = move $ Move RightApplication card slotNum
rightApplyRV slotNum value = moves $ applyRightVine slotNum value
achieveGoal destSlot value = moves $ toDo
    where
        availSlots = filter (/= destSlot) [2..8]
        (toDo, _) = runState (buildValue destSlot (translateValue value)) availSlots

getSlot dest 0 = do
  makeIdentity dest
  rightApply dest ZeroCard
  leftApply dest GetCard

getGameState :: MoveWriter GameState
getGameState = get

makeIdentity slotNum = do
  gs <- getGameState
  case gsGetField (gsMyFriend gs) gs slotNum of
    ValueCard IdentityCard -> return ()
    _ -> leftApply slotNum PutCard

