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

horizon = 100

giSlot (BuildGoal slot val) = slot
giSlot (TriggerGoal slot card) = slot

-- decide what to do.
thinkOfGoal :: GameState -> Goal
--thinkOfGoal gs = [ (0, (ValueApplication (grapeshot 512 0) (ValueNum 0)) ) ]
-- thinkOfGoal gs = [ (0, (grapeshot 512 0)) ]
thinkOfGoal gs = [ (BuildGoal 0 (ValueCard SCard) )]

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
thingsThatCouldMakeThis goal = [(previousNaiveGoal goal)]

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
                 then Move LeftApplication IdentityCard 0 -- we've reached enlightenment.
                 else breadthFirstSearch horizon gs [goal]

altPlanner :: PlayerModel
altPlanner = PurePlayer ((\x->[x]) . nextMove)

planBuildGoal :: GoalItem -> [(Move, Goal)]
-- "I could make this if I had..."
planBuildGoal (BuildGoal slot value)
  | value == (ValueCard IdentityCard) = [( Move LeftApplication PutCard slot , [] )] -- no prereq to empty a slot with put
  | isVine value = planVine slot value

planBuildGoal _ = [] -- "I don't see any way to do that."


-- like buildVine, but only to depth 1
planVine :: SlotNumber -> Value -> [(Move, Goal)]
planVine slot vine = planVine' vine
    where planVine' (ValueCard c) = [(Move RightApplication c slot, cleanSlateGoal)]
          planVine' (ValueNum v) = planNum v
          planVine' (ValueApplication (ValueCard c) v) = [(Move LeftApplication c slot  , inSlotGoal v)]
          planVine' (ValueApplication v (ValueCard c)) = [(Move RightApplication c slot , inSlotGoal v)]
          planVine' (ValueApplication v w) = error "planVine: not a vine"
          inSlotGoal value = [(BuildGoal slot value )]
          cleanSlateGoal = inSlotGoal (ValueCard IdentityCard)

-- I could make an N if I had N-1, or exactly N/2
planNum v = undefined
