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
type GoalItem = (SlotNumber, Value)

horizon = 100

-- decide what to do.
thinkOfGoal :: GameState -> [(SlotNumber, Value)]
--thinkOfGoal gs = [ (0, (ValueApplication (grapeshot 512 0) (ValueNum 0)) ) ]
thinkOfGoal gs = [ (0, (grapeshot 512 0)) ]
-- thinkOfGoal gs = [ (0, (ValueCard SCard) )]

naiveStepsToGoalItem :: (SlotNumber, Value) -> [Move]
naiveStepsToGoalItem (destSlot, thing) = fst $ runState (buildValue destSlot (translateValue thing)) [0..255]

naiveStepsToGoal goal = concatMap naiveStepsToGoalItem goal -- TODO: avoid conflicts

previousNaiveGoal goal = (last steps, gsToGoal prevGS)
  where prevGS = foldl (\gs move -> fst $ simulateTurn gs move) initialState (allButLast steps)
        steps = naiveStepsToGoal goal
        allButLast list = (inits list !! (length list - 1))

gsToGoal :: GameState -> Goal
gsToGoal gs = map slotToGoalItem [0..255]
  -- this is really fucking naive, it assumes everything must be exact.
  where slotToGoalItem num = (num, getSlotValue gs num)


breadthFirstSearch :: Int -> GameState -> [Goal] -> Move
breadthFirstSearch 0 _ _ = Move RightApplication ZeroCard 4
breadthFirstSearch limit gs goals = maybe deeper fst satisfiedGoal
  where choices = concatMap thingsThatCouldMakeThis goals
        -- if any choice's goal is satisfied, then we do the first move.
        satisfiedGoal = find ( metGoal gs . snd ) choices
        -- otherwise, lets see if we can get any of those prereqs
        deeper = breadthFirstSearch (limit - 1) gs $ map snd choices

thingsThatCouldMakeThis :: Goal -> [(Move, Goal)]
thingsThatCouldMakeThis goal = [(previousNaiveGoal goal)]

metGoal :: GameState -> Goal -> Bool
metGoal gs goal = all (metGoalItem gs) goal

-- Uh, maybe we'll need this, for pruning.
heuristicDistanceFromGoal :: GameState -> Goal -> Int
heuristicDistanceFromGoal gs goal = countFalses $ map (metGoalItem gs) goal
  where countFalses = foldl (\acc x -> acc + if x == False then 1 else 0 ) 0

metGoalItem :: GameState -> GoalItem -> Bool
metGoalItem gs (slot, value) = value == mySlotValue
  where mySlotValue = getSlotValue gs slot

getSlotValue gs = gsGetField ( gsMyFriend gs ) gs

nextMove :: GameState -> Move
nextMove gs = myMove
  where goal = thinkOfGoal gs
        myMove = if metGoal gs goal
                 then Move RightApplication ZeroCard 0 -- we've reached enlightenment.
                 else breadthFirstSearch horizon gs [goal]

altPlanner :: PlayerModel
altPlanner = PurePlayer ((\x->[x]) . nextMove)

