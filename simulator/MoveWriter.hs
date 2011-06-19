module MoveWriter where

import Control.Monad.Writer.Strict
import Control.Monad.State

import GameState
import Move
import Card
import Slots
import SimpleBuilder
import Simulator
import Value
import Translator
import Data.List

type MoveWriter = StateT GameState (WriterT [Move] (Either String))

execMoveWriter :: GameState -> MoveWriter () -> Maybe [Move]
execMoveWriter gs moveWriter = case execMoveWriterOrError gs moveWriter of
                                 Left _ -> Nothing
                                 Right moves -> Just moves

execMoveWriterOrError :: GameState -> MoveWriter () -> Either String [Move]
execMoveWriterOrError gs moveWriter = execWriterT (evalStateT moveWriter gs)

assertConstructionCost _ = return ()
assertSlotsUsed _ = return ()

-- Note: when applying moves, we simulate our turn but not zombie
-- actions.  So we won't have a perfect prediction of future game
-- state but hopefully it will be good enough to make plans from.
move :: Move -> MoveWriter ()
move m = do
  gs <- get
  case simulateTurn gs m of
    (gs', Right ()) -> do put gs'
                          tell [m]
    (gs', Left msg) -> lift $ lift $ Left $ unlines $
                       ["Error occurred while executing " ++ show m
                       ,"Game state was " ++ show gs
                       ,"Error message: " ++ msg
                       ]

moves :: [Move] -> MoveWriter()
moves = mapM_ move

leftApply slotNum card = move $ Move LeftApplication card slotNum
rightApply slotNum card = move $ Move RightApplication card slotNum
rightApplyRV slotNum value = moves $ applyRightVine slotNum value

assureSlotContains :: SlotNumber -> Value -> MoveWriter ()
assureSlotContains destSlot value = do
  forM_ slotsUsed makeIdentity
  moves toDo
    where
        availSlots = filter (/= destSlot) [2..8]
        (toDo, slotsLeft) = runState (buildValue destSlot (translateValue value)) availSlots
        slotsUsed = availSlots \\ slotsLeft

getSlot dest 0 = do
  makeIdentity dest
  rightApply dest ZeroCard
  leftApply dest GetCard

getGameState :: MoveWriter GameState
getGameState = get

makeIdentity slotNum = do
  gs <- getGameState
  case gsGetField (gsMyFriend gs) gs slotNum of
    ValueCard IdentityCard -> return ()
    _ -> leftApply slotNum PutCard

