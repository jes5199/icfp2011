module Value (Value(..),cardToValue,test_Value,valueI,valueZero,valueSucc,valueDbl,valueGet,valuePut,valueS,valueK,valueInc,valueDec,valueAttack,valueHelp,valueCopy,valueRevive,valueZombie,clamp,makeIntVal) where

import Test.HUnit
import Card

data Value = ValueCard Card
           | ValueNum Int
           | ValueApplication Value Value
           | ValueLambda String Value
           | ValueVariable String
           deriving (Eq)

instance Show Value where
  show (ValueCard c) = show c
  show (ValueNum n) = show n
  show (ValueApplication x y) = show x ++ "(" ++ show y ++ ")"
  show (ValueLambda s v) = "\\" ++ s ++ "." ++ show v
  show (ValueVariable s) = s

cardToValue :: Card -> Value
cardToValue = ValueCard

clamp val = if val < 0 then 0 else (if val > 65535 then 65535 else val)

makeIntVal val = ValueNum (clamp val)

valueI  = ValueCard IdentityCard
valueZero = ValueCard ZeroCard
valueSucc = ValueCard SuccCard
valueDbl = ValueCard DoubleCard
valueGet = ValueCard GetCard
valuePut = ValueCard PutCard
valueS = ValueCard SCard
valueK = ValueCard KCard
valueInc = ValueCard IncCard
valueDec = ValueCard DecCard
valueAttack = ValueCard AttackCard
valueHelp = ValueCard HelpCard
valueCopy = ValueCard CopyCard
valueRevive = ValueCard ReviveCard
valueZombie = ValueCard ZombieCard

test_Value = [
  ] :: [Test]
