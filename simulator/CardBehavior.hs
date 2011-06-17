module CardBehavior (test_CardBehavior) where

import Control.Monad.Error
import Test.HUnit
import GameState
import Value
import Card
import MoveStep

identity :: Value -> MoveStep Value
identity f = do incAppCount
                return f

zero :: MoveStep Value
zero = do incAppCount -- DOES ZERO COUNT AS A FUNCTION APPLICATION???
          return (ValueNum 0)

succ :: Value -> MoveStep Value
succ (ValueNum n) = do incAppCount
                       return $ ValueNum $ case n of
                         65535 -> 65535
                         _ -> n+1
succ _ = throwError "succ applied to non-number"

dbl :: Value -> MoveStep Value
dbl (ValueNum n) = do incAppCount
                      return $ ValueNum $ if n > 32767
                                          then 65535
                                          else n * 2
dbl _  = throwError "dbl applied to non-number"

get :: Value -> MoveStep Value
get = undefined

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
  (initialState,Right $ ValueCard IdentityCard)
  ] :: [Test]
