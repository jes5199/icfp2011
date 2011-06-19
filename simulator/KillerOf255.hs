module KillerOf255 where

import Control.Monad.Writer.Strict

import GameState
import Planner
import Move
import Card
import Parser
import Strategy

drive :: Drive
drive gs | gsGetVitality (gsMyEnemy gs) gs 255 > 0 = [GoalConj [OpponentSlotDead 255]]
drive _ = []

contractor gs (GoalConj [OpponentSlotDead 255]) = Just (FiniteCost (length speedKillTheMadBomberCell), speedKillTheMadBomberCell)
contractor _ _ = Nothing

type MoveWriter = Writer [Move]

assertConstructionCost _ = return ()
assertSlotsUsed _ = return ()

leftApply slotNum card = tell [Move LeftApplication card slotNum]
rightApply slotNum card = tell [Move RightApplication card slotNum]
rightApplyRV slotNum value = tell $ applyRightVine slotNum value

get dest 0 = do
  rightApply dest ZeroCard
  leftApply dest GetCard

speedKillTheMadBomberCell :: [Move]
speedKillTheMadBomberCell = execWriter $
    do assertConstructionCost 29
       assertSlotsUsed [128, 129]

       -- TODO: don't do this if unnecessary
       leftApply 0 PutCard

       --buildNumber 0 4 -- build 4 in 0
       rightApply 0 ZeroCard
       leftApply 0 SuccCard
       leftApply 0 DoubleCard
       leftApply 0 DoubleCard

       get 128 0 -- get from 0 to 128 (clear to identity first if necessary)

       --continueNumber 0 8 -- double the 4
       leftApply 0 DoubleCard

       get 129 0 -- get from 0 to 129 (clear to identity first if necessary)

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
