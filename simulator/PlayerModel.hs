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



getInterrupts :: PlayerModel -> GameState -> [Move] -> [Move]
getInterrupts (ExternalPlayer) _ _ = []
getInterrupts (PurePlayer _) gameState plan =
    revive 0 199 $
    revive 1 198 $
    revive 255 197 $
    revive 254 196 $
    revive_target_of_next_move $
    grabOur 255 197 $
    grabTheir 255 197 $
    grabOur 254 196 $
    grabTheir 254 196 $
    []
    where
        revive slot temp next = 
            if getSlotVitality gameState slot == 0
            then
                case findMe gameState (ValueNum slot) of
                Nothing  -> buildNum slot temp (getSlotValue gameState slot) next
                Just x   -> [ Move LeftApplication ReviveCard x] ++ buildVine x (ValueNum slot)
            else next
        revive_target_of_next_move next =
            case plan of
            (Move _ _ slot):_ -> revive slot 190 next
            _                 -> next
        buildNum n slot cur next
          | cur == (ValueNum n)                       = next -- Soft failure we were asked to build something that was already there
          | n == 0 && cur == (ValueCard IdentityCard) = [ Move RightApplication ZeroCard   slot ]
          | n == 0                                    = [ Move LeftApplication  PutCard    slot ]
          | cur == (ValueNum (n-1))                   = [ Move LeftApplication  SuccCard   slot ]
          | cur == (ValueNum (n `div` 2))             = [ Move LeftApplication  DoubleCard slot ]
          | otherwise                                 = buildNum (n `div` 2) slot cur next
        grabOur val loc next =
            if getSlotValue gameState 0 == (ValueNum val) && getSlotValue gameState loc /= (ValueNum val)
            then 
                case getSlotValue gameState loc of
                ValueCard IdentityCard -> [ Move RightApplication ZeroCard loc ]
                ValueNum 0             -> [ Move LeftApplication  GetCard  loc ]
                _                      -> []
            else next
        grabTheir val loc next =
            if getTheirSlotValue gameState 0 == (ValueNum val) && getSlotValue gameState loc /= (ValueNum val)
            then 
                case getSlotValue gameState loc of
                ValueCard IdentityCard -> [ Move RightApplication ZeroCard loc ]
                ValueNum 0             -> [ Move LeftApplication  CopyCard loc ]
                _                      -> []
            else []

getSlotVitality gs = gsGetVitality ( gsMyFriend gs ) gs
getSlotValue gs = gsGetField ( gsMyFriend gs ) gs
getTheirSlotValue gs = gsGetField ( gsMyEnemy gs ) gs

findMe :: GameState -> Value -> Maybe SlotNumber
findMe gs value = find (\x -> getSlotValue gs x == value ) (findLivingCells gs)

findLivingCells gs = filter ( \x -> getSlotVitality gs x > 0 ) [0..255]

