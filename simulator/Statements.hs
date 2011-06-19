module Statements where

import Value
import Card
import Slots
import Translator
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
                   valueS
                   (ValueApplication (ValueApplication valueS
                                      (ValueApplication valueK
                                       valuePut))
                    (funcValue f)))
                  valuePut)
                 True)

-- \x -> f(x); return g(x)
semi :: UnaryFunc -> UnaryFunc -> UnaryFunc
semi f g = -- S f g (assuming f returns I)
  UnaryFunc (ValueApplication (ValueApplication valueS
                               (funcValue(toStatement f))) (funcValue g))
  (isStatement g)

-- Bind a UnaryFunc to a constant input argument, forming a new
-- UnaryFunc that ignores its input.
bind :: UnaryFunc -> Value -> UnaryFunc
bind f value = -- S (K f) (K value)
  UnaryFunc (ValueApplication
             (ValueApplication valueS (ValueApplication valueK (funcValue f)))
             (ValueApplication valueK value))
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
              ValueApplication (ValueApplication valueS (funcValue (bind get nValue)))
                               (funcValue f)
    where nValue = translateValue (ValueNum n)
          get = statement valueGet

-- zombieLoop creates a loop that can only be used by zombies. This loop is based on
-- copy, rather than get. That's because zombies are dead, and so cannot get themselves.
-- But they can copy from you...
--
-- This also means that zombies don't destroy their weapons. You just lose the zombie and his
-- cheap trigger.
zombieLoop :: UnaryFunc -> SlotNumber -> Value
zombieLoop f localSlotWithCodeForNextIteration = -- the local slot to fetch will often be the slot with this loop, but not necessarily...
    ValueApplication (ValueApplication valueS (funcValue $ bind get nextSlot)) (funcValue f)
    where nextSlot = translateValue (ValueNum localSlotWithCodeForNextIteration)
          get = statement valueCopy

-- \x -> f(g(x))
compose :: UnaryFunc -> UnaryFunc -> UnaryFunc
compose f g = -- S (K f) g
  UnaryFunc (ValueApplication (ValueApplication valueS
                               (makeK (funcValue f))) (funcValue g))
  (isStatement f)

-- quine n f creates a statement that can be placed in slot n which
-- executes f but leaves the contents of slot n unchanged.
quine :: UnaryFunc -> SlotNumber -> Value
quine f n = -- S f (bind get n) (assuming f returns I)
  ValueApplication (ValueApplication valueS (funcValue (toStatement f)))
  (funcValue (bind get nValue))
    where nValue = translateValue (ValueNum n)
          get = statement valueGet

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

loneZombie :: Int -> SlotNumber -> SlotNumber -> Value
loneZombie weaponInput mySlotWithWeapon target =
    template "zombie target weapon" $ [("target", (ValueNum target)), ("weapon", trigger)]
    where
        trigger = ValueApplication (ValueApplication valueS (funcValue $ bind get nextSlot)) (ValueApplication valueK input)
        nextSlot = translateValue (ValueNum mySlotWithWeapon)
        input = ValueNum weaponInput
        get = statement valueCopy

goblinSapperBomb :: Int -> SlotNumber -> Value
goblinSapperBomb amount = zombieForLoop $
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

zombieForLoop :: UnaryFunc -> SlotNumber -> Value
zombieForLoop stuff = zombieLoop $
                  semi stuff
                  (routine (template "succ" []))

numericArgs = map numericArg
  where numericArg (name, i) = (name, (ValueNum i))

-- We'll need the ability to call a cell with an arbitrary parameter.
