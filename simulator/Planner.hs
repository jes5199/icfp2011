module Planner(makePlanner,Goal(..),Drive,Contractor) where

import Move
import Value
import GameState
import Card
import PlayerModel(PlayerModel(PurePlayer))

-- A goal represents a gamestate characteristic we would like to
-- achieve.
data Goal = SlotContains SlotNumber Value

-- A Drive is a function from GameState to a list of Goals.  The
-- planner will call all possible drives to figure out what goals it
-- should be pursuing.
type Drive = GameState -> [Goal]

-- A contractor is a function from Goal (and GameState) to either a
-- list of Moves, or Nothing (if the contractor doesn't know how to
-- make progress on the goal).
type Contractor = GameState -> Goal -> Maybe [Move]

makePlanner :: [Drive] -> [Contractor] -> PlayerModel
makePlanner drives contractors = PurePlayer (decide drives contractors)

-- The function you call to decide what to do
decide :: [Drive] -> [Contractor] -> GameState -> [Move]
decide drives contractors game =
    let moves = do drive <- drives
                   goal <- drive game
                   contractor <- contractors
                   let Just moves = contractor game goal
                   return moves
                ++ [[Move LeftApplication IdentityCard 0]]
    in head moves
