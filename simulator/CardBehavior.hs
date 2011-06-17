module CardBehavior (test_CardBehavior) where

import Test.HUnit
import GameState
import Function
import Card

type GameStep = GameState -> (GameState,Maybe Function)

identity :: Function -> GameStep
identity f state = (state,Just f)

zero :: GameStep
zero = undefined

succ :: Function -> GameStep
succ = undefined

dbl :: Function -> GameStep
dbl = undefined

get :: Function -> GameStep
get = undefined

put :: Function -> Function -> GameStep
put = undefined

s :: Function -> Function -> Function -> GameStep
s = undefined

k :: Function -> Function -> GameStep
k = undefined

inc :: Function -> GameStep
inc = undefined

dec :: Function -> GameStep
dec = undefined

attack :: Function -> Function -> Function -> GameStep
attack = undefined

help :: Function -> Function -> Function -> GameStep
help = undefined

copy :: Function -> GameStep
copy = undefined

revive :: Function -> GameStep
revive = undefined

zombie :: Function -> Function -> GameStep
zombie = undefined

test_CardBehavior = [
  identity (FunctionValue 3) initialState ~?= (initialState,Just $
                                                            FunctionValue 3),
  identity (FunctionCard IdentityCard) initialState ~?=
  (initialState,Just $ FunctionCard IdentityCard)
  
  ] :: [Test]
