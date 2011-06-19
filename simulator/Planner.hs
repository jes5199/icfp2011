module Planner(makePlanner,GoalConj(..),Drive,Contractor,GoalItem(..),Bid,Cost(..)) where

import Move
import Value
import GameState
import Card
import PlayerModel(PlayerModel(PurePlayer))
import Data.List

-- A GoalConj represents a set of gamestate characteristics we would
-- like to all achieve simultaneously.
newtype GoalConj = GoalConj [GoalItem]
data GoalItem = SlotContains SlotNumber Value
              | OpponentSlotDead SlotNumber -- slot number from opponent pov

-- A Drive is a function from GameState to a list of Goals.  The
-- planner will call all possible drives to figure out what goals it
-- should be pursuing.
type Drive = GameState -> [GoalConj]

-- A Cost is the number of moves it would take to achieve a thing.
data Cost = FiniteCost Int
          | InfiniteCost
    deriving (Eq, Ord)

-- For any given goal, a bid is the cost of achieving that goal, and
-- either (a) a set of moves which achieves that goal, or (b) a set of
-- moves which gets to an intermediate state from which the goal can
-- be achieved for (cost - num_moves) cost.
type Bid = (Cost, [Move])

-- A contractor is a function from GoalConj (and GameState) to either
-- a bid, or Nothing (if the contractor doesn't know how to make
-- progress on the goal).
type Contractor = GameState -> GoalConj -> Maybe Bid

makePlanner :: [Drive] -> [Contractor] -> PlayerModel
makePlanner drives contractors = PurePlayer (decide drives contractors)

-- The function you call to decide what to do
decide :: [Drive] -> [Contractor] -> GameState -> [Move]
decide drives contractors game =
    let bids =  do drive <- drives
                   goal <- drive game
                   contractor <- contractors
                   Just bid <- return (contractor game goal)
                   return bid
                ++ [(InfiniteCost, [Move LeftApplication IdentityCard 0])]
    in snd (head (sort bids))
