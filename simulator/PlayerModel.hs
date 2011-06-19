module PlayerModel where

import Control.Monad.State
import Data.List (minimumBy, find)
import SimpleBuilder
import Translator
import Value
import Card
import Slots
import Move
import GameState
import Parser

--
-- Player Model
--
-- A PlayerModel encapsulates collection functions that players use
-- to make choices about their moves
data PlayerModel = ExternalPlayer
                 | PurePlayer (GameState -> [Move])



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



getInterrupts :: PlayerModel -> GameState -> [Move]
getInterrupts (ExternalPlayer) _ = []
getInterrupts (PurePlayer _) gameState = restoreZero $ restoreOne $ []
  where restoreZero next = if getSlotVitality gameState 0 == 0
                              then case findMe gameState (ValueNum 0) of
                                     Nothing  -> [ Move RightApplication ZeroCard 199 ]
                                     Just x   -> [ Move LeftApplication ReviveCard x, Move RightApplication ZeroCard x ]
                              else next
        restoreOne  next = if getSlotVitality gameState 1 == 0
                              then case findMe gameState (ValueNum 1) of
                                     Nothing  -> repairOneIn198 $ getSlotValue gameState 198
                                     Just x   -> [ Move LeftApplication ReviveCard x, Move RightApplication ZeroCard x ]
                              else next
        repairOneIn198 v
          | v == (ValueNum 0)             = [ Move LeftApplication  SuccCard 198 ]
          | v == (ValueCard IdentityCard) = [ Move RightApplication ZeroCard 198 ]
          | otherwise                     = [ Move LeftApplication  PutCard  198 ]

getSlotVitality gs = gsGetVitality ( gsMyFriend gs ) gs
getSlotValue gs = gsGetField ( gsMyFriend gs ) gs

findMe :: GameState -> Value -> Maybe SlotNumber
findMe gs value = find (\x -> getSlotValue gs x == value ) (findLivingCells gs)

findLivingCells gs = filter ( \x -> getSlotVitality gs x > 0 ) [0..255]

