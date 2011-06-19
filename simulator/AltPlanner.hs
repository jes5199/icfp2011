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
import Planner


horizon = 100

-- decide what to do.
thinkOfGoal :: GameState -> GoalConj
--thinkOfGoal gs = [ (0, (ValueApplication (grapeshot 512 0) (ValueNum 0)) ) ]
-- thinkOfGoal gs = [ (0, (grapeshot 512 0)) ]
thinkOfGoal gs = GoalConj [ (SlotContains 0 (ValueNum 255) )]

gsToGoal :: GameState -> GoalConj
gsToGoal gs = GoalConj $ map slotToGoalItem [0..255]
  -- this is really fucking naive, it assumes everything must be exact.
  where slotToGoalItem num = SlotContains num (getSlotValue gs num)


breadthFirstSearch :: GameState -> [GoalConj] -> Maybe Bid
breadthFirstSearch gs = breadthFirstSearch' 0
  where breadthFirstSearch' depthSoFar _ | depthSoFar >= horizon = Nothing
        breadthFirstSearch' depthSoFar goals = maybe deeper extractBidFromSuccessfulSearch satisfiedGoal
            where choices = concatMap thingsThatCouldMakeThis goals
                  -- if any choice's goal is satisfied, then we do the first move.
                  satisfiedGoal = find ( metGoal gs . snd ) choices
                  -- otherwise, lets see if we can get any of those prereqs
                  deeper = breadthFirstSearch' (depthSoFar + 1) $ map snd choices
                  extractBidFromSuccessfulSearch (move, goal) = Just (FiniteCost depthSoFar, [move])

type WorkingBackwardsStep = (Move, GoalConj)

thingsThatCouldMakeThis :: GoalConj -> [WorkingBackwardsStep]
thingsThatCouldMakeThis (GoalConj goal) = concatMap planSlotContains goal

metGoal :: GameState -> GoalConj -> Bool
metGoal gs (GoalConj goal) = all (metGoalItem gs) goal

-- Uh, maybe we'll need this, for pruning.
heuristicDistanceFromGoal :: GameState -> GoalConj -> Int
heuristicDistanceFromGoal gs (GoalConj goal) = countFalses $ map (metGoalItem gs) goal
  where countFalses = foldl (\acc x -> acc + if x == False then 1 else 0 ) 0

metGoalItem :: GameState -> GoalItem -> Bool
metGoalItem gs (SlotContains slot value) = value == mySlotValue
  where mySlotValue = getSlotValue gs slot

getSlotValue gs = gsGetField ( gsMyFriend gs ) gs

drive :: Drive
drive gs = if metGoal gs goal
           then []
           else [Desire 0.1 goal]
    where goal = thinkOfGoal gs

contractor :: GameState -> GoalConj -> Maybe Bid
contractor gs goal = breadthFirstSearch gs [goal]

contractor2 :: Contractor
contractor2 gs (GoalConj [SlotContains slot value]) = Just $ (FiniteCost (length moves), moves)
    where moves = Move LeftApplication PutCard slot : fst (runState (buildValue slot value) tempSlots)
          tempSlots = [0..255] \\ [slot]
contractor2 _ _ = Nothing

--nextMove :: GameState -> Move
--nextMove gs = myMove
--  where goal = thinkOfGoal gs
--        myMove = if metGoal gs goal
--                 then Move RightApplication ZeroCard 0 -- full speed ahead.
--                 -- then Move LeftApplication IdentityCard 0 -- we've reached enlightenment.
--                 else breadthFirstSearch horizon gs [goal]

--altPlanner :: PlayerModel
--altPlanner = PurePlayer ((\x->[x]) . nextMove)

planSlotContains :: GoalItem -> [(Move, GoalConj)]
-- "I could make this if I had..."
planSlotContains (SlotContains slot value)
  | value == (ValueCard IdentityCard) = [( Move LeftApplication PutCard slot , GoalConj [] )] -- no prereq to empty a slot with put
  | isVine value = planVine slot value
planSlotContains _ = [] -- "I don't see any way to do that."

-- like buildVine, but only to depth 1
planVine :: SlotNumber -> Value -> [(Move, GoalConj)]
planVine slot vine = planVine' vine
    where planVine' (ValueCard c) = [(Move RightApplication c slot, cleanSlateGoal slot)]
          planVine' (ValueNum v) = planNum slot v
          planVine' (ValueApplication (ValueCard c) v) = [(Move LeftApplication c slot  , inSlotGoal slot v)]
          planVine' (ValueApplication v (ValueCard c)) = [(Move RightApplication c slot , inSlotGoal slot v)]
          planVine' (ValueApplication v w) = error "planVine: not a vine"

inSlotGoal slot value = GoalConj [(SlotContains slot value )]
cleanSlateGoal slot = inSlotGoal slot (ValueCard IdentityCard)

planNum :: SlotNumber -> Int -> [(Move, GoalConj)]
-- I could make a zero from an Identity
planNum slot 0 = [(Move RightApplication ZeroCard slot, cleanSlateGoal slot)]
-- I could make an N if I had N-1, or exactly N/2
planNum slot n = if n `mod` 2 == 0 then [ dbl, succ ] else [ succ ]
  where dbl  = (Move LeftApplication DoubleCard slot, inSlotGoal slot (ValueNum $ n `div` 2) )
        succ = (Move LeftApplication SuccCard    slot, inSlotGoal slot (ValueNum $ n - 1) )
-- TODO: I could make an N by getting it from elsewhere on the board
-- TODO: I could make an N by copying it from the opponent's board


