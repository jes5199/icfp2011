module PlayerModel where

import Control.Monad.State
import Strategy
import Value
import Card
import Move
import GameState
--import Simulator
import Parser
--import System(getArgs)

--
-- Player Model
--
-- A PlayerModel encapsulates collection functions that players use to make choices 
--     about their moves
data PlayerModel = PlayerModel { goalAgents :: [ GameState -> Goal ], priorityAgents :: [ Goal -> Int ] }



data Goal = TakeSpecificAction Move 
     |      BuildValue SlotNumber Value
     |      ReviveCell SlotNumber
     |      AttackCell SlotNumber

--
-- Goal Agents
--

-- Make a specfic thing, ignoring game state
gaMakeThisAt what targetCell game_state = BuildValue targetCell (translateValue (parse what))

-- TODO: read and parse the action they took and assume that doing that was their goal
--     (Note: this should be expressed as a "do this", not acomplish this) 
gaExternal game_state = TakeSpecificAction (Move LeftApplication IdentityCard 0)



--
-- Choose best goal based on all sorts of clever logic
--      For now, use "first thing on the list"
--
chooseGoal brain game_state =
    let goals = ($game_state) `map` (goalAgents brain)
    in head goals

--
-- Figure out the next steps(s) to accomplish a given goal
--
planSteps (TakeSpecificAction m) _ = [m]
planSteps other game_state = planStepsBlindly other game_state


-- Blindly do the whole thing ignoring state
planStepsBlindly (BuildValue loc val) game_state = fst $ runState (buildValue loc val) [1..255]


