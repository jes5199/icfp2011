module CardBehavior (test_CardBehavior) where

import Control.Monad.Error (throwError)
import Test.HUnit
import GameState
import Value
import Card
import MoveStep

identity :: Value -> MoveStep Value
identity x = do incAppCount
                return x

zero :: MoveStep Value
zero = return (ValueNum 0)

successor :: Value -> MoveStep Value
successor (ValueNum n) = do incAppCount
                            return $ ValueNum $ case n of
                              65535 -> 65535
                              _ -> n+1
successor _ = throwError succNANmsg
-- definition is separate for benefit of unit test
succNANmsg = "succ applied to non-number"

dbl :: Value -> MoveStep Value
dbl (ValueNum n) = do incAppCount
                      return $ ValueNum $ if n > 32767
                                          then 65535
                                          else n * 2
dbl _  = throwError dblNANmsg
dblNANmsg = "dbl applied to non-number"

get :: Value -> MoveStep Value
get (ValueNum i) = do incAppCount
                      if i >= 0 && i <= 255
                        then getProponentSlotField i
                        else throwError getRangeMsg
get _ = throwError getNANmsg
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
  runMove (identity (ValueNum 3)) initialState ~?=
  (initialState,Right $ ValueNum 3),
  runMove (identity (ValueCard IdentityCard)) initialState ~?=
  (initialState,Right $ ValueCard IdentityCard),

  runMove zero initialState ~?= (initialState,Right $ ValueNum 0),

  runMove (successor (ValueNum 3)) initialState ~?=
  (initialState,Right $ ValueNum 4),
  runMove (successor (ValueNum 0)) initialState ~?=
  (initialState,Right $ ValueNum 1),
  runMove (successor (ValueNum 65534)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (successor (ValueNum 65535)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (successor (ValueCard IdentityCard)) initialState ~?=
  (initialState,Left succNANmsg),

  runMove (dbl (ValueNum 0)) initialState ~?=
  (initialState,Right $ ValueNum 0),
  runMove (dbl (ValueNum 7)) initialState ~?=
  (initialState,Right $ ValueNum 14),
  runMove (dbl (ValueNum 32767)) initialState ~?=
  (initialState,Right $ ValueNum 65534),
  runMove (dbl (ValueNum 32768)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (dbl (ValueNum 65535)) initialState ~?=
  (initialState,Right $ ValueNum 65535),
  runMove (dbl (ValueCard IdentityCard)) initialState ~?=
  (initialState,Left dblNANmsg),

  runMove (get (ValueNum 0)) initialState ~?=
  (initialState,Right $ ValueCard IdentityCard),
  runMove (get (ValueNum 255)) initialState ~?=
  (initialState,Right $ ValueCard IdentityCard),
  runMove (get (ValueNum 256)) initialState ~?=
  (initialState,Left getRangeMsg),
  runMove (get (ValueCard IdentityCard)) initialState ~?=
  (initialState,Left getNANmsg)
  ]
