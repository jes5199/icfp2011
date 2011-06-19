module PlayerModel where

import Control.Monad.State
import Data.List (minimumBy)
import Strategy
import Value
import Card
import Move
import GameState
import Parser

--
-- Player Model
--
-- A PlayerModel encapsulates collection functions that players use
-- to make choices about their moves
data PlayerModel = ModeledPlayer { goalAgents :: [ GameState -> [Goal] ],
                                   priorityAgents :: [ Goal -> Int ] }
                 | ExternalPlayer
                 | PurePlayer (GameState -> Move)



data Goal = TakeSpecificAction Move
     |      BuildValue SlotNumber Value
     |      BuildValueSomewhere Value
     |      ReviveCell SlotNumber
     |      AttackCell SlotNumber
  deriving Show

--
-- Goal Agents
--

-- Make a specific thing at a specific location, unless it's already there
gaMakeThisAt :: String -> SlotNumber -> GameState -> [Goal]
gaMakeThisAt what targetCell game_state =
    let
       structure = parse what
       structure' = translateValue structure
       current_contents = (gsGetField (gsMyFriend game_state)) game_state targetCell
    in
    if current_contents == structure || current_contents == structure' then []
    else [BuildValue targetCell structure']

-- TODO: read and parse the action they took and assume that doing that was
-- their goal
--     (Note: this should be expressed as a "do this", not accomplish this)
gaIForever :: GameState -> [Goal]
gaIForever game_state = [TakeSpecificAction (Move LeftApplication IdentityCard 0)]

gaReviveTheDead = undefined
gaHealTheWounded = undefined


--
-- Choose best goal based on all sorts of clever logic
--      For now, use "first thing on the list"
--
chooseGoal :: PlayerModel -> GameState -> Goal
chooseGoal brain game_state =
    let goals = concat (($game_state) `map` (goalAgents brain))
    in head goals

--
-- Figure out the next steps(s) to accomplish a given goal
-- For now, do the thing that takes the fewest number of turns
--
planSteps :: Goal -> GameState -> [Move]
planSteps (TakeSpecificAction m) _ = [m]
planSteps goal game_state =
  shortestSequence [
    planStepsBlindly goal game_state
    ]

shortestSequence :: [[a]] -> [a]
shortestSequence xs = minimumBy (\x y -> compare (length x) (length y)) xs

-- Blindly do the whole thing ignoring state
planStepsBlindly :: Goal -> GameState -> [Move]
planStepsBlindly (BuildValue loc val) game_state = fst $ runState (buildValue loc val) [1..255]
