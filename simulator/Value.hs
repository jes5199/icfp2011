module Value (Value(..),test_Value,valueI,valueZero,valueSucc,valueDbl,valueGet,valuePut,valueS,valueK,valueInc,valueDec,valueAttack,valueHelp,valueCopy,valueRevive,valueZombie,makeNumVal) where

import Test.HUnit
import Card
import Util

data Value = ValueCard Card
           | ValueNum Int
           | ValueApplication Value Value
           | ValueLambda String Value
           | ValueVariable String

instance Eq Value where
    ValueCard x          == ValueCard y           =  x == y
    ValueNum x           == ValueNum y            =  x == y
    ValueApplication f x == ValueApplication g y  =  f == g && x == y
    ValueLambda x v      == ValueLambda y w       =  x == y && v == w
    ValueVariable x      == ValueVariable y       =  x == y
    ValueCard ZeroCard   == ValueNum 0            =  True
    ValueNum 0           == ValueCard ZeroCard    =  True
    _                    == _                     =  False

instance Show Value where
  show (ValueCard c) = show c
  show (ValueNum n) = show n
  show (ValueApplication x y) = show x ++ "(" ++ show y ++ ")"
  show (ValueLambda s v) = "\\" ++ s ++ "." ++ show v
  show (ValueVariable s) = s

makeNumVal :: Int -> Value
makeNumVal val = ValueNum (clampInt 0 65535 val)

valueI      = ValueCard IdentityCard
valueZero   = ValueCard ZeroCard
valueSucc   = ValueCard SuccCard
valueDbl    = ValueCard DoubleCard
valueGet    = ValueCard GetCard
valuePut    = ValueCard PutCard
valueS      = ValueCard SCard
valueK      = ValueCard KCard
valueInc    = ValueCard IncCard
valueDec    = ValueCard DecCard
valueAttack = ValueCard AttackCard
valueHelp   = ValueCard HelpCard
valueCopy   = ValueCard CopyCard
valueRevive = ValueCard ReviveCard
valueZombie = ValueCard ZombieCard

test_Value = [
 (valueI == valueI) ~?= True,
 (valueI == valueSucc) ~?= False,
 (valueZero == ValueNum 0) ~?= True,
 (valueZero == ValueNum 1) ~?= False,
 (ValueNum 0 == valueZero) ~?= True,
 (ValueNum 1 == valueZero) ~?= False,
 (valueSucc == ValueNum 0) ~?= False,
 (valueSucc == ValueNum 1) ~?= False,
 (ValueNum 0 == valueSucc) ~?= False,
 (ValueNum 1 == valueSucc) ~?= False,
 (ValueApplication valueK valueZero == ValueApplication valueK valueZero) ~?= True,
 (ValueApplication valueS valueZero == ValueApplication valueK valueZero) ~?= False,
 (ValueApplication valueK valueSucc == ValueApplication valueK valueZero) ~?= False,
 (ValueLambda "x" valueK == ValueLambda "x" valueK) ~?= True,
 (ValueLambda "y" valueK == ValueLambda "x" valueK) ~?= False,
 (ValueLambda "x" valueS == ValueLambda "x" valueK) ~?= False,
 (ValueVariable "x" == ValueVariable "x") ~?= True,
 (ValueVariable "x" == ValueVariable "y") ~?= False,
 (ValueVariable "x" == ValueLambda "x" valueI) ~?= False,
 (ValueApplication valueSucc valueZero == ValueApplication valueSucc valueZero) ~?= True
  ] :: [Test]
