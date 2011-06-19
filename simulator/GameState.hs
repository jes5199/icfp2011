module GameState (Who(..), opponent,
                  GameState(..), initialState, alterFirstBoard,
                  switchPlayer, beginZombieApocolypse, quellZombieApocolypse,
                  GSPerspective, perspectiveFor, gsMyFriend, gsMyEnemy,
                  gsGetVitality, gsGetField,
                  gsSetField, gsPayVitalityCost, gsApplyVitalityConsequence,
                  gsSetVitalityOnDeadSlot,
                  test_GameState) where

import Test.HUnit
import Data.Array
import Card
import Value
import Slots
import Move
import Util


data Who = FirstPlayer | SecondPlayer
         deriving (Eq, Show)

opponent FirstPlayer = SecondPlayer
opponent SecondPlayer = FirstPlayer


data GameState = GameState { playerToMove :: Who,
                             firstPlayerSlots :: Slots,
                             secondPlayerSlots :: Slots,
                             zombiesAreOut :: Bool }
               deriving (Eq)

instance Show GameState where
  show gs = show (playerToMove gs) ++ " to move" ++
            (if zombiesAreOut gs then " (zombies)" else "") ++ "\n" ++
            "First player slots:\n" ++ show (firstPlayerSlots gs) ++
            "Second player slots:\n" ++ show (secondPlayerSlots gs)

initialState :: GameState
initialState = GameState FirstPlayer initialSide initialSide False

-- This function is an abomination used only for testing (almost)
alterFirstBoard :: (Slots -> Slots) -> GameState -> GameState
alterFirstBoard transform gs =
  gs { firstPlayerSlots = transform (firstPlayerSlots gs) }

switchPlayer :: GameState -> GameState
switchPlayer state = state { playerToMove = opponent (playerToMove state) }

beginZombieApocolypse :: GameState -> GameState
beginZombieApocolypse gs = gs { zombiesAreOut = True }

quellZombieApocolypse :: GameState -> GameState
quellZombieApocolypse gs = gs { zombiesAreOut = False }


-- This is a perspective on the board, as viewed by some player or zombie.
-- "player1's view of player 2's board" means actor is FirstPlayer,
-- viewer is SecondPlayer.
data GSPerspective = GSPerspective { viewer :: Who,
                                     zombieApocolypse :: Bool }
                   deriving Eq

instance Show GSPerspective where
  show pers = if zombieApocolypse pers
              then "zombies are swarming " ++ person
              else person ++ " is acting"
                where
                  person = show (viewer pers)

-- The perspective of a particular player.
perspectiveFor :: Who -> Bool -> GSPerspective
perspectiveFor = GSPerspective

-- The current actor's friend. An actor is a player or a zombie master.
gsMyFriend :: GameState -> GSPerspective
gsMyFriend (GameState who p1 p2 zombies) =
    perspectiveFor who zombies

-- The current actor's enemy. An actor is a player or a zombie master.
gsMyEnemy :: GameState -> GSPerspective
gsMyEnemy (GameState who p1 p2 zombies) =
    perspectiveFor (opponent who) zombies

gsGetSlots :: GSPerspective -> GameState -> Slots
gsGetSlots p
  | viewer p == FirstPlayer  = firstPlayerSlots
  | viewer p == SecondPlayer = secondPlayerSlots

gsGetVitality :: GSPerspective -> GameState -> SlotNumber -> Vitality
gsGetVitality pers = extractVitality . gsGetSlots pers

gsGetField :: GSPerspective -> GameState -> SlotNumber -> Value
gsGetField pers = extractField . gsGetSlots pers

gsSetSlots :: Slots -> GSPerspective -> GameState -> GameState
gsSetSlots s p gs
  | viewer p == FirstPlayer  = gs { firstPlayerSlots  = s }
  | viewer p == SecondPlayer = gs { secondPlayerSlots = s }

gsTransformSlots :: (Slots -> Slots) ->
                    GSPerspective -> GameState -> GameState
gsTransformSlots f p gs = gsSetSlots (f $ gsGetSlots p gs) p gs

type GSTrans = GSPerspective -> GameState -> GameState

gsSetField :: SlotNumber -> Value -> GSTrans
gsSetField idx value = gsTransformSlots (updateField value idx)

gsPayVitalityCost :: SlotNumber -> Vitality -> GSTrans
gsPayVitalityCost idx cost =
  gsTransformSlots (changeVitalityInSlot idx (-cost))

gsApplyVitalityConsequence :: SlotNumber -> Vitality -> GSTrans
gsApplyVitalityConsequence idx deltaHp pers =
  gsTransformSlots
  (changeVitalityInSlot idx (if zombieApocolypse pers
                             then (-deltaHp)
                             else deltaHp)) pers

gsSetVitalityOnDeadSlot :: SlotNumber -> Vitality -> GSTrans
gsSetVitalityOnDeadSlot idx hp =
  gsTransformSlots (replaceVitalityOnDeadSlot idx hp)

test_GameState = [
    -- Begin tests to get the right perspectives
    gsMyFriend startingGame ~?= player1,
    gsMyFriend (switchPlayer startingGame) ~?= player2,
    gsMyFriend zombieTime ~?= zombie1,
    gsMyFriend (switchPlayer zombieTime) ~?= zombie2,
    gsMyEnemy startingGame ~?= player2,
    gsMyEnemy (switchPlayer startingGame) ~?= player1,
    gsMyEnemy zombieTime ~?= zombie2,
    gsMyEnemy (switchPlayer zombieTime) ~?= zombie1,
    -- End tests to get the right perspectives

    -- Begin tests of perspective behaviors
    gsGetVitality player1 startingGame 3 ~?= 8000,
    gsGetVitality player2 startingGame 3 ~?= 12000,
    gsGetField player1 startingGame 0 ~?= valueHelp,
    gsGetField player2 startingGame 0 ~?= valueAttack,

    gsGetVitality zombie1 zombieTime 3 ~?= 8000,
    gsGetVitality zombie2 zombieTime 3 ~?= 12000,
    gsGetField zombie1 zombieTime 0 ~?= valueHelp,
    gsGetField zombie2 zombieTime 0 ~?= valueAttack,

    gsGetVitality player1 (gsPayVitalityCost 3 100 player1 startingGame) 3 ~?= 7900,
    gsGetVitality player2 (gsPayVitalityCost 3 100 player2 startingGame) 3 ~?= 11900,
    gsGetVitality player1 (gsPayVitalityCost 3 100 zombie1 zombieTime) 3 ~?= 7900,
    gsGetVitality player2 (gsPayVitalityCost 3 100 zombie2 zombieTime) 3 ~?= 11900,

    gsGetVitality player1 (gsApplyVitalityConsequence 3 (-100) player1 startingGame) 3 ~?= 7900,
    gsGetVitality player2 (gsApplyVitalityConsequence 3 100 player2 startingGame) 3 ~?= 12100,
    gsGetVitality player1 (gsApplyVitalityConsequence 3 (-100) zombie1 zombieTime) 3 ~?= 8100,
    gsGetVitality player2 (gsApplyVitalityConsequence 3 100 zombie2 zombieTime) 3 ~?= 11900,

    gsGetField player1 (gsSetField 0 valueZombie player1 startingGame) 0 ~?= valueZombie,
    gsGetField player2 (gsSetField 0 valueRevive player2 startingGame) 0 ~?= valueRevive,

    gsGetField player1 (gsSetField 0 valueZombie zombie1 zombieTime) 0 ~?= valueZombie,
    gsGetField player2 (gsSetField 0 valueRevive zombie2 zombieTime) 0 ~?= valueRevive,

    gsGetVitality player1 (gsSetVitalityOnDeadSlot 3 1 player1 someDeath) 3 ~?= 1,
    gsGetVitality player2 (gsSetVitalityOnDeadSlot 3 (-1) player2 someDeath) 3 ~?= (-1),
    gsGetVitality player1 (gsSetVitalityOnDeadSlot 3 1 zombie1 someDeath) 3 ~?= 1,
    gsGetVitality player2 (gsSetVitalityOnDeadSlot 3 (-1) zombie2 someDeath) 3 ~?= (-1),
    -- End tests of perspective behaviors

    -- Begin boundary check tests
    gsGetVitality player1 (gsApplyVitalityConsequence 3 (-10000) player1 startingGame) 3 ~?= 0,
    gsGetVitality player1 (gsApplyVitalityConsequence 3 70000 player1 startingGame) 3 ~?= 65535,
    gsGetVitality player1 (gsSetVitalityOnDeadSlot 3 1 player1 startingGame) 3 ~?= 8000,
    gsGetVitality player1 (gsApplyVitalityConsequence 3 5000 player1 someDeath) 3 ~?= 0
    -- End boundary check tests
    ]
  where
    firstSide = updateField valueHelp 0 (updateVitality 8000 3 initialSide)
    secondSide = updateField valueAttack 0 (updateVitality 12000 3 initialSide)
    deadCellSide = updateVitality 0 3 initialSide
    startingGame = GameState FirstPlayer firstSide secondSide False
    zombieTime = GameState FirstPlayer firstSide secondSide True
    someDeath = GameState FirstPlayer deadCellSide deadCellSide True
    player1 = perspectiveFor FirstPlayer False
    player2 = perspectiveFor SecondPlayer False
    zombie1 = perspectiveFor FirstPlayer True
    zombie2 = perspectiveFor SecondPlayer True
