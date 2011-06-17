module CardBehavior (test_CardBehavior) where

import Test.HUnit
import GameState
import Value
import Card

type GameStep = GameState -> (GameState,Maybe Value)

identity :: Value -> GameStep
identity f state = (state,Just f)

zero :: GameStep
zero state = (state, Just $ ValueNum 0)

succ :: Value -> GameStep
succ (ValueNum n) state = (state, Just $ ValueNum m)
  where m = case n of
              65535 -> 65535
              _ -> n+1
succ _ state = (state, Nothing)

dbl :: Value -> GameStep
dbl (ValueNum n) state = (state, Just $ ValueNum m)
  where m = if n > 32767
              then 65535
              else n * 2
dbl _ state = (state, Nothing)

get :: Value -> GameStep
get = undefined

put :: Value -> Value -> GameStep
put = undefined

s :: Value -> Value -> Value -> GameStep
s = undefined

k :: Value -> Value -> GameStep
k = undefined

inc :: Value -> GameStep
inc = undefined

dec :: Value -> GameStep
dec = undefined

attack :: Value -> Value -> Value -> GameStep
attack = undefined

help :: Value -> Value -> Value -> GameStep
help = undefined

copy :: Value -> GameStep
copy = undefined

revive :: Value -> GameStep
revive = undefined

zombie :: Value -> Value -> GameStep
zombie = undefined

test_CardBehavior = [
  identity (ValueNum 3) initialState ~?= (initialState,Just $ ValueNum 3),
  identity (ValueCard IdentityCard) initialState ~?=
  (initialState,Just $ ValueCard IdentityCard)
  ] :: [Test]
