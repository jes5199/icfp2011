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

grapeshot :: Int -> SlotNumber -> Value
grapeshot damage = forLoop $ statement (template "\\i -> attack i i damage" $ numericArgs [("damage", damage)] )

firingSquad :: Int -> SlotNumber -> SlotNumber -> Value
firingSquad damage target = forLoop $ statement (template "\\i -> attack i target damage" $ numericArgs [("damage", damage), ("target", target) ])

heal :: SlotNumber -> Int -> SlotNumber -> Value
heal target amount = infLoop $
  bind ( statement $ template "help target target" $ numericArgs [("target", target)] ) (ValueNum amount)

spreadLove :: Int -> SlotNumber -> Value
spreadLove amount = forLoop $
  statement (template "\\i -> help i (succ i) amount" $ numericArgs [("amount", amount) ])

cureLightWounds :: Int -> SlotNumber -> Value
cureLightWounds amount = forLoop $
  statement (template "\\i -> help i i amount" $ numericArgs [("amount", amount) ])

-- Note: this doesn't qualify as a "spell" by the strict definition
fastKill :: SlotNumber -> SlotNumber -> SlotNumber -> Value
fastKill friend1 friend2 enemy =
  funcValue $ semi (statement (template "attack friend enemy 4096" $ numericArgs [("friend", friend1), ("enemy", enemy) ]))
                   (statement (template "attack friend enemy 8192" $ numericArgs [("friend", friend2), ("enemy", enemy) ]))

massRaiseDead:: SlotNumber -> Value
massRaiseDead = forLoop $ statement (template "\\i -> revive i" [] )

massResurrection:: Int -> SlotNumber -> Value
massResurrection value = forLoop $
  semi ( statement (template "\\i -> revive (succ i)" [] ) )
       ( statement (template "\\i -> help i (succ i) value" $ numericArgs [("value", value)] ) )

forLoop :: UnaryFunc -> SlotNumber -> Value
forLoop stuff = infLoop $
                  semi stuff
                  (routine (template "succ" []))

numericArgs = map numericArg
  where numericArg (name, i) = (name, (ValueNum i))

-- We'll need the ability to call a cell with an arbitrary parameter.
