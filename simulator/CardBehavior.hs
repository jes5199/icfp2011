module CardBehavior (test_CardBehavior) where

import Test.HUnit
import GameState
import Value
import Card

type GameStep = GameState -> (GameState,Maybe Value)

identity :: Value -> GameStep
identity f state = (state,Just f)

zero :: GameStep
zero = undefined

succ :: Value -> GameStep
succ = undefined

dbl :: Value -> GameStep
dbl = undefined

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
