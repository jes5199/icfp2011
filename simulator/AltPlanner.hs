module AltPlanner where

import Control.Monad.State
import Data.List

import Card
import Statements
import Simulator
import GameState
import Move -- move is Move Application Card SlotNumber
import Value
import Strategy
import PlayerModel(PlayerModel(PurePlayer))

type Goal = [GoalItem]
data GoalItem = BuildGoal SlotNumber Value
              | TriggerGoal SlotNumber Card
              | AnywhereGoal Value

horizon = 100

-- decide what to do.
thinkOfGoal :: GameState -> Goal
--thinkOfGoal gs = [ (0, (ValueApplication (grapeshot 512 0) (ValueNum 0)) ) ]
-- thinkOfGoal gs = [ (0, (grapeshot 512 0)) ]
thinkOfGoal gs = [ (BuildGoal 0 (ValueNum 255) )]

naiveStepsToGoalItem :: GoalItem -> [Move]
naiveStepsToGoalItem (TriggerGoal _ _) = []
naiveStepsToGoalItem (BuildGoal destSlot (ValueCard IdentityCard) ) = []
naiveStepsToGoalItem (BuildGoal destSlot thing) = fst $ runState (buildValue destSlot (translateValue thing)) [0..255]

--naiveStepsToGoal goal = concatMap naiveStepsToGoalItem goal -- TODO: avoid conflicts
naiveStepsToGoal :: Goal -> [Move]
naiveStepsToGoal goal = naiveStepsToGoalItem $ head goal

previousNaiveGoal goal = (last steps, gsToGoal prevGS)
  where prevGS = if (length steps) > 1
                 then foldl (\gs move -> fst $ simulateTurn gs move) initialState (allButLast 1 steps)
                 else initialState
        steps = naiveStepsToGoal goal
        allButLast n list = (inits list !! (length list - n))

gsToGoal :: GameState -> Goal
gsToGoal gs = map slotToGoalItem [0..255]
  -- this is really fucking naive, it assumes everything must be exact.
  where slotToGoalItem num = BuildGoal num (getSlotValue gs num)


breadthFirstSearch :: Int -> GameState -> [Goal] -> Move
breadthFirstSearch 0 _ _ = Move LeftApplication IdentityCard 42
breadthFirstSearch limit gs goals = maybe deeper fst satisfiedGoal
  where choices = concatMap thingsThatCouldMakeThis goals
        -- if any choice's goal is satisfied, then we do the first move.
        satisfiedGoal = find ( metGoal gs . snd ) choices
        -- otherwise, lets see if we can get any of those prereqs
        deeper = breadthFirstSearch (limit - 1) gs $ map snd choices

thingsThatCouldMakeThis :: Goal -> [(Move, Goal)]
thingsThatCouldMakeThis goal = concatMap planBuildGoal goal

metGoal :: GameState -> Goal -> Bool
metGoal gs goal = all (metGoalItem gs) goal

-- Uh, maybe we'll need this, for pruning.
heuristicDistanceFromGoal :: GameState -> Goal -> Int
heuristicDistanceFromGoal gs goal = countFalses $ map (metGoalItem gs) goal
  where countFalses = foldl (\acc x -> acc + if x == False then 1 else 0 ) 0

metGoalItem :: GameState -> GoalItem -> Bool
metGoalItem gs (TriggerGoal _ _) = False
metGoalItem gs (BuildGoal slot value) = value == mySlotValue
  where mySlotValue = getSlotValue gs slot

getSlotValue gs = gsGetField ( gsMyFriend gs ) gs

nextMove :: GameState -> Move
nextMove gs = myMove
  where goal = thinkOfGoal gs
        myMove = if metGoal gs goal
                 then Move RightApplication ZeroCard 0 -- full speed ahead.
                 -- then Move LeftApplication IdentityCard 0 -- we've reached enlightenment.
                 else breadthFirstSearch horizon gs [goal]

altPlanner :: PlayerModel
altPlanner = PurePlayer ((\x->[x]) . nextMove)

planBuildGoal :: GoalItem -> [(Move, Goal)]
-- "I could make this if I had..."
planBuildGoal (BuildGoal slot value)
  | value == (ValueCard IdentityCard) = [( Move LeftApplication PutCard slot , [] )] -- no prereq to empty a slot with put
  | isVine value = planVine slot value
-- planBuildGoal (BuildGoal slot (ValueApplication f x))
-- | isRightVine x = planVine slot
--  | isRightVine x = do moves1 <- buildValue destSlot f
--                       let moves2 = applyRightVine destSlot x
--                       return $ moves1 ++ moves2
--  | otherwise = do moves1 <- buildValue destSlot f
--                   availableSlots <- get
--                   let slotToUse = head availableSlots
--                   put $ tail availableSlots
--                   moves2 <- buildValue slotToUse x
--                   let moves3 = applyRightVine destSlot $ translateValue $ ValueApplication (ValueCard GetCard) (ValueNum slotToUse)
--                   return $ moves1 ++ moves2 ++ moves3

planBuildGoal _ = [] -- "I don't see any way to do that."

-- like buildVine, but only to depth 1
planVine :: SlotNumber -> Value -> [(Move, Goal)]
planVine slot vine = planVine' vine
    where planVine' (ValueCard c) = [(Move RightApplication c slot, cleanSlateGoal slot)]
          planVine' (ValueNum v) = planNum slot v
          planVine' (ValueApplication (ValueCard c) v) = [(Move LeftApplication c slot  , inSlotGoal slot v)]
          planVine' (ValueApplication v (ValueCard c)) = [(Move RightApplication c slot , inSlotGoal slot v)]
          planVine' (ValueApplication v w) = error "planVine: not a vine"

inSlotGoal slot value = [(BuildGoal slot value )]
cleanSlateGoal slot = inSlotGoal slot (ValueCard IdentityCard)

planNum :: SlotNumber -> Int -> [(Move, Goal)]
-- I could make a zero from an Identity
planNum slot 0 = [(Move RightApplication ZeroCard slot, cleanSlateGoal slot)]
-- I could make an N if I had N-1, or exactly N/2
planNum slot n = if n `mod` 2 == 0 then [ dbl, succ ] else [ succ ]
  where dbl  = (Move LeftApplication DoubleCard slot, inSlotGoal slot (ValueNum $ n `div` 2) )
        succ = (Move LeftApplication SuccCard    slot, inSlotGoal slot (ValueNum $ n - 1) )
-- TODO: I could make an N by getting it from elsewhere on the board
-- TODO: I could make an N by copying it from the opponent's board


