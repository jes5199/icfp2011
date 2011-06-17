module CardBehavior (test_CardBehavior) where

import Control.Monad.Error (throwError)
import Test.HUnit
import GameState
import Value
import Card
import MoveStep

doIdentity :: Value -> MoveStep Value
doIdentity x = return x

doZero :: MoveStep Value
doZero = return (ValueNum 0)

doSucc :: Value -> MoveStep Value
doSucc (ValueNum n) = return $ ValueNum $ if n == 65535
                                          then 65535
                                          else n+1
doSucc _ = throwError succNANmsg
-- definition is separate for benefit of unit test
succNANmsg = "succ applied to non-number"

doDbl :: Value -> MoveStep Value
doDbl (ValueNum n) = return $ ValueNum $ if n > 32767
                                         then 65535
                                         else n * 2
doDbl _  = throwError dblNANmsg
dblNANmsg = "dbl applied to non-number"

doGet :: Value -> MoveStep Value
doGet (ValueNum i) = if i >= 0 && i <= 255
                     then getProponentSlotField i
                     else throwError getRangeMsg
doGet _ = throwError getNANmsg
getRangeMsg = "get out of range"
getNANmsg = "get applied to non-number"

put :: Value -> Value -> MoveStep Value
put = undefined

s :: Value -> Value -> Value -> MoveStep Value
s = undefined

k :: Value -> Value -> MoveStep Value
k = undefined

inc :: Value -> MoveStep Value
inc = undefined

dec :: Value -> MoveStep Value
dec = undefined

attack :: Value -> Value -> Value -> MoveStep Value
attack = undefined

help :: Value -> Value -> Value -> MoveStep Value
help = undefined

copy :: Value -> MoveStep Value
copy = undefined

revive :: Value -> MoveStep Value
revive = undefined

zombie :: Value -> Value -> MoveStep Value
zombie = undefined

test_CardBehavior = [
  runMove (doIdentity (ValueNum 3)) initialState ~?=
  (initialState,Right $ ValueNum 3),
  runMove (doIdentity (ValueCard IdentityCard)) initialState ~?=
  (initialState,Right $ ValueCard IdentityCard),

  runMove doZero initialState ~?= (initialState,Right $ ValueNum 0),

  runMove (doSucc (ValueNum 3)) initialState ~?=
  (initialState,Right $ ValueNum 4),
  runMove (doSucc (ValueNum 0)) initialState ~?=
  (initialState,Right $ ValueNum 1),
  runMove (doSucc (ValueNum 65534)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (doSucc (ValueNum 65535)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (doSucc (ValueCard IdentityCard)) initialState ~?=
  (initialState,Left succNANmsg),

  runMove (doDbl (ValueNum 0)) initialState ~?=
  (initialState,Right $ ValueNum 0),
  runMove (doDbl (ValueNum 7)) initialState ~?=
  (initialState,Right $ ValueNum 14),
  runMove (doDbl (ValueNum 32767)) initialState ~?=
  (initialState,Right $ ValueNum 65534),
  runMove (doDbl (ValueNum 32768)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (doDbl (ValueNum 65535)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (doDbl (ValueCard IdentityCard)) initialState ~?=
  (initialState,Left dblNANmsg),

  runMove (doGet (ValueNum 0)) initialState ~?=
  (initialState,Right $ ValueCard IdentityCard),
  runMove (doGet (ValueNum 255)) initialState ~?=
  (initialState,Right $ ValueCard IdentityCard),
  runMove (doGet (ValueNum 256)) initialState ~?=
  (initialState,Left getRangeMsg),
  runMove (doGet (ValueCard IdentityCard)) initialState ~?=
  (initialState,Left getNANmsg)
  ]
