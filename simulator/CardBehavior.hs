module CardBehavior (test_CardBehavior,apply) where

import Control.Monad.Error (throwError)
import Test.HUnit
import GameState
import Value
import Card
import MoveStep


apply :: Value -> Value -> MoveStep Value
apply (ValueNum _) _ = throwError applyNumMsg
apply (ValueCard IdentityCard) arg = incAppCount >> doI arg
apply (ValueCard ZeroCard) arg = throwError arityMsg
apply (ValueCard SuccCard) arg = incAppCount >> doSucc arg
apply (ValueCard DoubleCard) arg = incAppCount >> doDbl arg
apply (ValueCard GetCard) arg = incAppCount >> doGet arg
apply (ValueCard PutCard) arg = incAppCount >> doPut arg
apply (ValueApplication (ValueApplication (ValueCard SCard) f) g) x =
  incAppCount >> doS f g x
apply ff@(ValueApplication (ValueCard SCard) f) g =
  incAppCount >> return (ValueApplication ff g)
apply ss@(ValueCard SCard) f =
  incAppCount >> return (ValueApplication ss f)
apply k@(ValueCard KCard) x =
  incAppCount >> return (ValueApplication k x)
apply (ValueApplication (ValueCard KCard) x) y = incAppCount >> doK x y
apply (ValueCard IncCard) i = incAppCount >> doInc i

apply x y = error (show x ++ " APPLIED TO " ++ show y)
-- need more


applyNumMsg = "Number on left of application"
arityMsg = "Something applied to too many args"

doI :: Value -> MoveStep Value
doI x = return x

doZero :: MoveStep Value
doZero = return $ ValueNum 0

doSucc :: Value -> MoveStep Value
doSucc (ValueNum n) = return $ ValueNum $ if n == 65535
                                          then 65535
                                          else n+1
doSucc (ValueCard ZeroCard) = return $ ValueNum 1
doSucc _ = throwError succNANmsg
-- definition is separate for benefit of unit test
succNANmsg = "succ applied to non-number"

doDbl :: Value -> MoveStep Value
doDbl (ValueNum n) = return $ ValueNum $ if n > 32767
                                         then 65535
                                         else n * 2
doDbl _  = throwError dblNANmsg
dblNANmsg = "dbl applied to non-number"

doGet :: Value -> MoveStep Value
doGet (ValueNum i) = if i >= 0 && i <= 255
                     then getProponentField i
                     else throwError getRangeMsg
doGet _ = throwError getNANmsg
getRangeMsg = "get out of range"
getNANmsg = "get applied to non-number"

doPut :: Value -> MoveStep Value
doPut _ = return $ ValueCard IdentityCard

doS :: Value -> Value -> Value -> MoveStep Value
doS f g x = do h <- apply f x
               y <- apply g x
               z <- apply h y
               return z

doK :: Value -> Value -> MoveStep Value
doK x y = return x

doInc :: Value -> MoveStep Value
doInc (ValueNum i) = if i >= 0 && i <= 255
                     then do v <- getProponentVitality i
                             let v' = case v of
                                   65535 -> 65535
                                   0 -> 0
                                   -1 -> -1
                                   _ -> v+1
                             putProponentVitality v' i
                             return $ ValueCard IdentityCard
                     else throwError incRangeMsg
doInc _ = throwError incNANmsg
incRangeMsg = "inc out of range"
incNANmsg = "inc applied to non-number"

doDec :: Value -> MoveStep Value
doDec = undefined

doAttack :: Value -> Value -> Value -> MoveStep Value
doAttack = undefined

doHelp :: Value -> Value -> Value -> MoveStep Value
doHelp = undefined

doCopy :: Value -> MoveStep Value
doCopy = undefined

doRevive :: Value -> MoveStep Value
doRevive = undefined

doZombie :: Value -> Value -> MoveStep Value
doZombie = undefined

test_CardBehavior = [
  runMove (doI (ValueNum 3)) initialState ~?=
    (initialState,Right $ ValueNum 3),
  runMove (doI (ValueCard IdentityCard)) initialState ~?=
    (initialState,Right $ ValueCard IdentityCard),

  runMove doZero initialState ~?= (initialState,Right $ ValueNum 0),

  runMove (doSucc (ValueNum 3)) initialState ~?=
    (initialState,Right $ ValueNum 4),
  runMove (doSucc (ValueNum 0)) initialState ~?=
    (initialState,Right $ ValueNum 1),
  runMove (doSucc (ValueNum 65534)) initialState ~?=
    (initialState,Right $ ValueNum 65535),
  runMove (doSucc (ValueNum 65535)) initialState ~?=
    (initialState,Right $ ValueNum 65535),
  runMove (doSucc (ValueCard IdentityCard)) initialState ~?=
    (initialState,Left succNANmsg),
  runMove (doSucc (ValueCard ZeroCard)) initialState ~?=
    (initialState,Right $ ValueNum 1),

  runMove (doDbl (ValueNum 0)) initialState ~?=
    (initialState,Right $ ValueNum 0),
  runMove (doDbl (ValueNum 7)) initialState ~?=
    (initialState,Right $ ValueNum 14),
  runMove (doDbl (ValueNum 32767)) initialState ~?=
    (initialState,Right $ ValueNum 65534),
  runMove (doDbl (ValueNum 32768)) initialState ~?=
    (initialState,Right $ ValueNum 65535),
  runMove (doDbl (ValueNum 65535)) initialState ~?=
    (initialState,Right $ ValueNum 65535),
  runMove (doDbl (ValueCard IdentityCard)) initialState ~?=
    (initialState,Left dblNANmsg),

  runMove (doGet (ValueNum 0)) initialState ~?=
    (initialState,Right $ ValueCard IdentityCard),
  runMove (doGet (ValueNum 255)) initialState ~?=
    (initialState,Right $ ValueCard IdentityCard),
  runMove (doGet (ValueNum 256)) initialState ~?=
    (initialState,Left getRangeMsg),
  runMove (doGet (ValueCard IdentityCard)) initialState ~?=
    (initialState,Left getNANmsg),

  runMove (doPut (ValueNum 345)) initialState ~?=
    (initialState,Right $ ValueCard IdentityCard),
  runMove (doPut (ValueCard SCard)) initialState ~?=
    (initialState,Right $ ValueCard IdentityCard),

  runMove (doS (ValueNum 0) (ValueCard IdentityCard) (ValueCard IdentityCard))
  initialState ~?=
    (initialState,Left applyNumMsg),
  runMove (doS (ValueCard IdentityCard) (ValueNum 0) (ValueCard IdentityCard))
  initialState ~?=
    (initialState,Left applyNumMsg),
  runMove (doS (ValueCard IdentityCard) (ValueCard IdentityCard) (ValueNum 0))
  initialState ~?=
    (initialState,Left applyNumMsg),
  runMove (doS (ValueCard IdentityCard) (ValueCard IdentityCard)
           (ValueCard IdentityCard))
  initialState ~?=
    (initialState,Right $ ValueCard IdentityCard),
  -- Paul made this example
  runMove (doS (ValueApplication (ValueCard KCard) (ValueCard SuccCard))
           (ValueCard SuccCard) (ValueCard ZeroCard)) initialState ~?=
    (initialState,Right $ ValueNum 2),
  runMove (apply (ValueApplication
                  (ValueApplication
                   (ValueCard SCard)
                   (ValueApplication
                    (ValueCard KCard)
                    (ValueCard SuccCard)))
                  (ValueCard SuccCard))
           (ValueCard ZeroCard)) initialState ~?=
    (initialState,Right $ ValueNum 2),

  runMove (doK (ValueNum 3) (ValueNum 6)) initialState ~?=
    (initialState,Right $ ValueNum 3),
  runMove (doK (ValueCard SCard) (ValueCard SuccCard)) initialState ~?=
    (initialState,Right $ ValueCard SCard),

  runMove (doInc (ValueCard IdentityCard)) initialState ~?=
    (initialState,Left incNANmsg),
  runMove (doInc (ValueNum 256)) initialState ~?=
    (initialState,Left incRangeMsg),
  runMove (doInc (ValueNum 0)) initialState ~?=
    (GameState FirstPlayer (updateVitality 10001 0 initialSide) initialSide,
     Right $ ValueCard IdentityCard),
  runMove (doInc (ValueNum 0)) (GameState FirstPlayer (updateVitality 0 0 initialSide) initialSide) ~?=
    (GameState FirstPlayer (updateVitality 0 0 initialSide) initialSide,
     Right $ ValueCard IdentityCard),
  runMove (doInc (ValueNum 0)) (GameState FirstPlayer (updateVitality 65535 0 initialSide) initialSide) ~?=
    (GameState FirstPlayer (updateVitality 65535 0 initialSide) initialSide,
     Right $ ValueCard IdentityCard)
  {-
  -- Infinite loop example
  runMove (doS (ValueCard IdentityCard) (ValueCard IdentityCard)
           (ValueCard SuccCard))
  initialState ~?=
    (initialState,Right $ ValueCard IdentityCard) -}
  ]
