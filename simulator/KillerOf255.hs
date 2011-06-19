module KillerOf255 where

import GameState
import Planner
import Card
import Parser
import MoveWriter
import Statements

makeStrategy condition desire implemetation = (drive, contractor)
    where
        drive gs | (condition gs) = desire
        drive _ = []
        contractor gs goal
            = do GoalConj objective <- return goal
                 case (implemetation objective) of
                    Just moveWriter -> do
                         moves <- execMoveWriterOrError gs moveWriter
                         return (FiniteCost (length moves), moves)
                    _ -> Left $ "I don't know how to handle " ++ show objective 

setUpTheBomb = makeStrategy
    (\gs -> gsGetVitality (gsMyEnemy gs) gs 255 > 0)
    ([Desire 100.0 (GoalConj [OpponentSlotDead 255])])
    (\objective -> case objective of
        [OpponentSlotDead 255] -> Just speedKillTheMadBomberCell
        _ -> Nothing)

strategies :: [Strategy]
strategies = [setUpTheBomb]

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
