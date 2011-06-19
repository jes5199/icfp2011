module KillerOf255 where

import Control.Monad.Writer.Strict(WriterT, execWriterT)
import qualified Control.Monad.Writer.Strict as Writer -- So that we don't accidentally call tell
import Control.Monad.State

import GameState
import Planner
import Move
import Card
import Parser
import Strategy
import Simulator

drive :: Drive
drive gs | gsGetVitality (gsMyEnemy gs) gs 255 > 0 = [Desire 100.0 (GoalConj [OpponentSlotDead 255])]
drive _ = []

contractor gs goal
    = do GoalConj [OpponentSlotDead 255] <- return goal
         moves <- speedKillTheMadBomberCell gs
         return (FiniteCost (length moves), moves)

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
  Writer.tell [m]

moves :: [Move] -> MoveWriter()
moves = mapM_ move

leftApply slotNum card = move $ Move LeftApplication card slotNum
rightApply slotNum card = move $ Move RightApplication card slotNum
rightApplyRV slotNum value = moves $ applyRightVine slotNum value

getSlot dest 0 = do
  rightApply dest ZeroCard
  leftApply dest GetCard

speedKillTheMadBomberCell :: GameState -> Maybe [Move]
speedKillTheMadBomberCell gs = execMoveWriter gs $
    do assertConstructionCost 29
       assertSlotsUsed [128, 129]

       -- TODO: don't do this if unnecessary
       leftApply 0 PutCard

       --buildNumber 0 4 -- build 4 in 0
       rightApply 0 ZeroCard
       leftApply 0 SuccCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard

       getSlot 128 0 -- get from 0 to 128 (clear to identity first if necessary)

       --continueNumber 0 8 -- double the 4
       leftApply 0 DoubleCard

       getSlot 129 0 -- get from 0 to 129 (clear to identity first if necessary)

       --continueNumber 0 4096
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard

       leftApply 128 AttackCard
       rightApply 128 ZeroCard
       rightApplyRV 128 (parse "get 0") -- should fire the gun. I don't want it to build something that will delay fire. I didn't say to bind it or make it lazy or anything.

       leftApply 0 DoubleCard

       leftApply 129 AttackCard
       rightApply 129 ZeroCard
       rightApplyRV 129 (parse "get 0") -- should fire the gun. I don't want it to build something that will delay fire. I didn't say to bind it or make it lazy or anything.
