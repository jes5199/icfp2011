module KillerOf255 where

import GameState
import Planner
import Card
import Parser
import MoveWriter
import Statements

drive :: Drive
drive gs | gsGetVitality (gsMyEnemy gs) gs 255 > 0 = [Desire 100.0 (GoalConj [OpponentSlotDead 255])]
drive _ = []

contractor :: Contractor
contractor gs goal
    = do GoalConj [OpponentSlotDead 255] <- return goal
         moves <- execMoveWriterOrError gs speedKillTheMadBomberCell
         return (FiniteCost (length moves), moves)

strategy :: Strategy
strategy = (drive, contractor)

goblinSappersAtLowEnd :: MoveWriter ()
goblinSappersAtLowEnd =
    do assureSlotContains 1 (goblinSapperBomb 8192 1)
       assureSlotContains 130 (loneZombie 0 1 0)

speedKillTheMadBomberCell :: MoveWriter ()
speedKillTheMadBomberCell =
    do assertConstructionCost 29
       assertSlotsUsed [128, 129]

       makeIdentity 0

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

doublePunch :: MoveWriter ()
doublePunch =
  do rightApply 0 ZeroCard
     leftApply  0 SuccCard
     leftApply  0 DoubleCard
     leftApply  0 SuccCard
     leftApply  0 DoubleCard
     leftApply  0 SuccCard
     leftApply  0 DoubleCard
     leftApply  0 SuccCard
     leftApply  0 DoubleCard
     leftApply  0 SuccCard
     leftApply  0 DoubleCard
     leftApply  0 SuccCard
     leftApply  0 DoubleCard
     rightApply 1 ZeroCard
     leftApply 1 GetCard
     leftApply 0 SuccCard
     rightApply 2 ZeroCard
     leftApply 2 GetCard
     leftApply 1 AttackCard
     leftApply 2 AttackCard
     leftApply 1 KCard
     leftApply 2 KCard
     leftApply 1 SCard
     leftApply 2 SCard
     rightApply 1 GetCard
     rightApply 2 GetCard
     leftApply 0 DoubleCard
     leftApply 0 SuccCard
     rightApply 1 ZeroCard
     rightApply 2 ZeroCard
     leftApply 1 KCard
     leftApply 2 KCard
     leftApply 1 SCard
     leftApply 2 SCard
     rightApply 1 GetCard
     rightApply 2 GetCard
     leftApply 0 DoubleCard
     leftApply 0 DoubleCard
     leftApply 0 DoubleCard
     leftApply 0 DoubleCard
     leftApply 0 DoubleCard
     rightApply 1 ZeroCard
     rightApply 2 ZeroCard

