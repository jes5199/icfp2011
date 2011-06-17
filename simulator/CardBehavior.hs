module CardBehavior (test_CardBehavior) where

import Control.Monad.Error (throwError)
import Test.HUnit
import GameState
import Value
import Card
import MoveStep

doI :: Value -> MoveStep Value
doI x = return x

doZero :: MoveStep Value
doZero = return $ ValueNum 0

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

doPut :: Value -> MoveStep Value
doPut _ = return $ ValueCard IdentityCard

doS :: Value -> Value -> Value -> MoveStep Value
doS = undefined

doK :: Value -> Value -> MoveStep Value
doK = undefined

doInc :: Value -> MoveStep Value
doInc = undefined

doDec :: Value -> MoveStep Value
doDec = undefined

doAttack :: Value -> Value -> Value -> MoveStep Value
doAttack = undefined

doHelp :: Value -> Value -> Value -> MoveStep Value
doHelp = undefined

doCopy :: Value -> MoveStep Value
doCopy = undefined

doRevive :: Value -> MoveStep Value
doRevive = undefined

doZombie :: Value -> Value -> MoveStep Value
doZombie = undefined

test_CardBehavior = [
  runMove (doI (ValueNum 3)) initialState ~?=
    (initialState,Right $ ValueNum 3),
  runMove (doI (ValueCard IdentityCard)) initialState ~?=
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
    (initialState,Left getNANmsg),

  runMove (doPut (ValueNum 345)) initialState ~?=
    (initialState,Right $ ValueCard IdentityCard),
  runMove (doPut (ValueCard SCard)) initialState ~?=
    (initialState,Right $ ValueCard IdentityCard)
  ]
