module Planner where

import Move
import Value
import GameState
import Card

-- A goal represents a gamestate characteristic we would like to
-- achieve.
data Goal = SlotContains SlotNumber Value

-- A Drive is a function from GameState to a list of Goals.  The
-- planner will call all possible drives to figure out what goals it
-- should be pursuing.
type Drive = GameState -> [Goal]

-- A contractor is a function from Goal (and GameState) to either a
-- Move, or Nothing (if the contractor doesn't know how to make
-- progress on the goal).
type Contractor = GameState -> Goal -> Maybe Move

-- The function you call to decide what to do
decide :: [Drive] -> [Contractor] -> GameState -> Move
decide drives contractors game =
    let moves = do drive <- drives
                   goal <- drive game
                   contractor <- contractors
                   let Just move = contractor game goal
                   return move
                ++ [Move LeftApplication IdentityCard 0]
    in head moves
