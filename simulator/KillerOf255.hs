module KillerOf255 where

import GameState
import Planner
import Card
import Parser
import MoveWriter
import Statements
import Control.Monad
import Slots

makeVariableStrategy :: Drive -> ([GoalItem] -> GameState -> Maybe (MoveWriter ())) -> Strategy
makeVariableStrategy drive implementation = (drive, contractor)
    where
        contractor gs goal
            = do GoalConj objective <- return goal
                 case (implementation objective gs) of
                    Just moveWriter -> do
                         moves <- execMoveWriterOrError gs moveWriter
                         return (FiniteCost (length moves), moves)
                    _ -> Left $ "I don't know how to handle " ++ show objective

makeStrategy :: (GameState -> Bool) -> [Desire] -> ([GoalItem] -> Maybe (MoveWriter ())) -> Strategy
makeStrategy condition desire implementation = makeVariableStrategy drive implementation'
    where
        drive gs | (condition gs) = desire
        drive _ = []
        implementation' objective gs = implementation objective

isAlive perspective slotNum = \gs -> gsGetVitality (perspective gs) gs slotNum > 0
isDead perspective slotNum = \gs -> gsGetVitality (perspective gs) gs slotNum == 0
enoughHp perspective minHp slotNum = \gs -> gsGetVitality (perspective gs) gs slotNum >= minHp
hpBelow perspective maxHp slotNum = \gs -> gsGetVitality (perspective gs) gs slotNum < maxHp

mine = gsMyFriend
his = gsMyEnemy

allTrue :: [GameState -> Bool] -> GameState -> Bool
allTrue conditions gs = all (\f -> f gs) conditions

setUpTheBomb = makeStrategy
    (allTrue [(isAlive his 255), (enoughHp mine 4096 4), (enoughHp mine 8192 8)])
    [Desire 100.0 (GoalConj [OpponentSlotDead 255])]
    (\objective -> case objective of
        [OpponentSlotDead 255] -> Just speedKillTheMadBomberCell
        _ -> Nothing)

clearTheBeaches = makeStrategy
    (allTrue ([(isDead his 255)] ++ (map (enoughHp his 8192) [128..130])))
    [Desire 150.0 (GoalConj [OpponentSlotsDeadStartingAt 128])]
    (\objective -> case objective of
        [OpponentSlotsDeadStartingAt startPos] -> Just (goblinSappers startPos)
        _ -> Nothing)

screwUpTheirRegisters = makeStrategy
    (allTrue ([(isDead his 255)] ++ (map (enoughHp his 8192) [0..2])))
    [Desire 100.0 (GoalConj [OpponentSlotsDeadStartingAt 0])]
    (\objective -> case objective of
        [OpponentSlotsDeadStartingAt startPos] -> Just (goblinSappers startPos)
        _ -> Nothing)

doublePunchForce = 8160

doublePunchStrategy = makeStrategy
    (allTrue [(isAlive his 0), (enoughHp mine doublePunchForce 126), (enoughHp mine doublePunchForce 127)])
    ([Desire 110.0 (GoalConj [OpponentSlotDead 0] )])
    (\objective -> case objective of
        [OpponentSlotDead 0] -> Just doublePunch
        _ -> Nothing)

strategies :: [Strategy]
strategies = [setUpTheBomb, clearTheBeaches, screwUpTheirRegisters, doublePunchStrategy, healerStrategy]

healerStrategy = makeVariableStrategy
    (\gs -> goalsFor gs [0..8] 10000 ++ goalsFor gs [9..255] 2000)
    (\objective gs -> case objective of
                     [ProponentSlotHealedBy slot amount] -> Just $ healer slot amount
                     _ -> Nothing)
    where goalsFor gs slots targetHealth = do
            slot <- slots
            let health = gsGetVitality (gsMyFriend gs) gs slot
            guard (health < targetHealth && health > 1000)
            let urgency = 200.0 * toFloating (targetHealth - health) / toFloating targetHealth
            return $ Desire urgency (GoalConj [ProponentSlotHealedBy slot (powTwoBelow health)])
          toFloating = fromInteger . toInteger
          powTwoBelow 0 = 0
          powTwoBelow 1 = 1
          powTwoBelow n = 2 * (powTwoBelow (n `div` 2))

healer :: SlotNumber -> Int -> MoveWriter ()
healer target amount = do
  assureSlotContains 8 (heal target amount 8)
  rightApply 8 ZeroCard

goblinSappersAtLowEnd :: MoveWriter ()
goblinSappersAtLowEnd =
    do assureSlotContains 1 (goblinSapperBomb 8192 1)
       assureSlotContains 130 (loneZombie 0 1 0)

goblinSappers :: SlotNumber -> MoveWriter ()
goblinSappers startingIndex =
    do assureSlotContains 1 (goblinSapperBomb 8192 1)
       assureSlotContains 130 (loneZombie startingIndex 1 0)

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

