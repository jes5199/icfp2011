module Statements where

import Value
import Card
import Strategy
import Move

-- A UnaryFunc encapsulates a value for which:
-- (a) we know it takes one argument, and
-- (b) we know whether or not it returns I.
data UnaryFunc = UnaryFunc { funcValue :: Value, isStatement :: Bool }

-- Construct a UnaryFunc based on a Value that is known to return I.
-- Such a UnaryFunc is called a "statement".
statement :: Value -> UnaryFunc
statement f = UnaryFunc f True

-- Construct a UnaryFunc based on a Value that is not known to return
-- I.  Such a UnaryFunc is called a "routine".
routine :: Value -> UnaryFunc
routine f = UnaryFunc f False

-- Modify a UnaryFunc if necessary to guarantee that it is a
-- statement.
toStatement :: UnaryFunc -> UnaryFunc
toStatement f | isStatement f = f
              | otherwise = -- S (S (K put) f) put
                            (UnaryFunc
                             (ValueApplication
                              (ValueApplication
                               (ValueCard SCard)
                               (ValueApplication (ValueApplication (ValueCard SCard)
                                                                   (ValueApplication (ValueCard KCard)
                                                                                     (ValueCard PutCard)))
                                                 (funcValue f)))
                              (ValueCard PutCard))
                             True)

-- \x -> f(x); return g(x)
semi :: UnaryFunc -> UnaryFunc -> UnaryFunc
semi f g = -- S f g (assuming f returns I)
           UnaryFunc (ValueApplication (ValueApplication (ValueCard SCard) (funcValue (toStatement f))) (funcValue g))
                     (isStatement g)

-- Bind a UnaryFunc to a constant input argument, forming a new
-- UnaryFunc that ignores its input.
bind :: UnaryFunc -> Value -> UnaryFunc
bind f value = -- S (K f) (K value)
               UnaryFunc (ValueApplication
                          (ValueApplication (ValueCard SCard) (ValueApplication (ValueCard KCard) (funcValue f)))
                          (ValueApplication (ValueCard KCard) value))
                         (isStatement f)

-- infLoop n stmt creates a statement that can be placed in slot n
-- which executes stmt in an infinite loop.
--
-- When you run an infLoop, you get to pass a card into it, and that
-- card is applied to f on the first iteration.  Thereafter, the value
-- that gets applied to f is whatever f returned in the previous
-- iteration.  THIS CAN BE USED TO MAKE COUNTERS.
--
-- infLoop n stmt has type Value, not Statement, because you can't
-- compose it with other statements safely.
infLoop :: UnaryFunc -> SlotNumber -> Value
infLoop f n = -- S (bind get n) f
              ValueApplication (ValueApplication (ValueCard SCard) (funcValue (bind get nValue)))
                               (funcValue f)
    where nValue = translateValue (ValueNum n)
          get = statement (ValueCard GetCard)

-- \x -> f(g(x))
compose :: UnaryFunc -> UnaryFunc -> UnaryFunc
compose f g = -- S (K f) g
              UnaryFunc (ValueApplication (ValueApplication (ValueCard SCard) (makeK (funcValue f))) (funcValue g))
                        (isStatement f)

-- quine n f creates a statement that can be placed in slot n which
-- executes f but leaves the contents of slot n unchanged.
quine :: UnaryFunc -> SlotNumber -> Value
quine f n = -- S f (bind get n) (assuming f returns I)
            ValueApplication (ValueApplication (ValueCard SCard) (funcValue (toStatement f)))
                             (funcValue (bind get nValue))
    where nValue = translateValue (ValueNum n)
          get = statement (ValueCard GetCard)

grapeShot :: SlotNumber -> Value
grapeShot = infLoop $
            semi (statement (lambda "i" theAttack))
                 (routine (ValueCard SuccCard))
    where theAttack = (ValueApplication
                       (ValueApplication (ValueApplication (ValueCard AttackCard) (ValueVariable "i"))
                                         (ValueVariable "i"))
                       (ValueNum 8192))

lambda varName value = translateValue (ValueLambda varName value)

-- We'll need the ability to call a cell with an arbitrary parameter.