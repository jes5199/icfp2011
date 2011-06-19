module Spells () where

import Tactics

speedKillTheMadBomberCell gs | ((field gs 0 ~?= valueI) && (vitality gs 8 ~?> 8192) && (vitality gs 4 ~?> 4096) && (vitalityEnemy gs 255 ~?< 10984)) =
    do assertConstructionCost 29
       assertSlotsUsed [128, 129]
       buildNumber 0 4 -- build 4 in 0
       get 128 0 -- get from 0 to 128 (clear to identity first if necessary)
       continueNumber 0 8 -- double the 4
       get 129 0 -- get from 0 to 129 (clear to identity first if necessary)
       continueNumber 0 4096
       leftApply 128 AttackCard
       rightApply 128 ZeroCard
       rightApply 128 (parse "get 0") -- should fire the gun. I don't want it to build something that will delay fire. I didn't say to bind it or make it lazy or anything.
       leftApply 129 AttackCard
       rightApply 129 ZeroCard
       rightApply 129 (parse "get 0") -- should fire the gun. I don't want it to build something that will delay fire. I didn't say to bind it or make it lazy or anything.

speedKillTheMadBomberCell gs | ((field gs 0 ~?= 8096) && (vitality gs 32 ~?> 8192) && (vitality gs 16 ~?> 4096) && (field gs 1 ~?= valueI) && (vitalityEnemy gs 255 ~?< 10984)) =
    do assertConstructionCost 22
       assertSlotsUsed [1, 2]
       buildNumber 1 16
       get 2 1
       continueNumber 1 32
       leftApply 1 AttackCard
       rightApply 1 ZeroCard
       rightApply 1 (parse "get 0")
       leftApply 2 AttackCard
       rightApply 2 ZeroCard
       rightApply 2 (parse "get 0")
