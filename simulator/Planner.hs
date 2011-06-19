module Planner(makePlanner,GoalConj(..),Drive,Contractor,GoalItem(..),Bid,Cost(..),Desire(..),Strategy) where

import Move
import Value
import GameState
import Card
import Slots
import PlayerModel(PlayerModel(PurePlayer))
import Data.List

-- A GoalConj represents a set of gamestate characteristics we would
-- like to all achieve simultaneously.
newtype GoalConj = GoalConj [GoalItem]
    deriving (Eq, Show)

data GoalItem = SlotContains SlotNumber Value
              | OpponentSlotDead SlotNumber -- slot number from opponent pov
              | OpponentSlotsDeadStartingAt SlotNumber -- slot number from opponent pov
    deriving (Eq, Show)

-- A desire is a GoalConj paired with a ranking.  Larger numbers are
-- more desirable.
data Desire = Desire Double GoalConj
    deriving (Eq, Show)

instance Ord Desire where
    compare (Desire x _) (Desire y _) = compare x y

-- A Drive is a function from GameState to a list of Goals.  The
-- planner will call all possible drives to figure out what goals it
-- should be pursuing.
type Drive = GameState -> [Desire]

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
-- an error message indicating why it can't make a bid on this goal,
-- or a bid.
type Contractor = GameState -> GoalConj -> Either String Bid

-- A strategy is a convenient bundle containing a drive and a
-- contractor.
type Strategy = (Drive, Contractor)

makePlanner :: [Drive] -> [Contractor] -> [Strategy] -> PlayerModel
makePlanner drives contractors strategies = PurePlayer (decide (drives ++ (map fst strategies)) (contractors ++ (map snd strategies)))

-- The function you call to decide what to do
decide :: [Drive] -> [Contractor] -> GameState -> [Move]
decide drives contractors game =
    let bids =  do drive <- drives
                   Desire desirability goal <- drive game
                   contractor <- contractors
                   Right bid <- return (contractor game goal)
                   return (-desirability, bid)
                ++ [(100.0, (InfiniteCost, [Move LeftApplication IdentityCard 0]))]
    in snd (snd (head (sort bids)))
