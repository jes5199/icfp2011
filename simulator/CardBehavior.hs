module CardBehavior (test_CardBehavior,apply) where

import Control.Monad.Error (throwError)
import Test.HUnit
import GameState
import Value
import Card
import MoveStep

validSlot :: Int -> MoveStep ()
validSlot c = if c >= 0 && c <= 255
              then return ()
              else throwError invalidSlotError
invalidSlotError = "slot number out of range"

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

apply (ValueCard DecCard) i = incAppCount >> doDec i

apply (ValueApplication (ValueApplication (ValueCard AttackCard) i) j) n =
  incAppCount >> doAttack i j n
apply ii@(ValueApplication (ValueCard AttackCard) i) j =
  incAppCount >> return (ValueApplication ii j)
apply card@(ValueCard AttackCard) i =
  incAppCount >> return (ValueApplication card i)

apply (ValueApplication (ValueApplication (ValueCard HelpCard) i) j) n =
  incAppCount >> doHelp i j n
apply ii@(ValueApplication (ValueCard HelpCard) i) j =
  incAppCount >> return (ValueApplication ii j)
apply card@(ValueCard HelpCard) i =
  incAppCount >> return (ValueApplication card i)

apply (ValueCard CopyCard) i = incAppCount >> doCopy i

apply (ValueCard ReviveCard) i = incAppCount >> doRevive i

apply (ValueApplication (ValueCard ZombieCard) i) x =
  incAppCount >> doZombie i x
apply card@(ValueCard ZombieCard) i =
  incAppCount >> return (ValueApplication card i)

apply x y = error (show x ++ " APPLIED TO " ++ show y)


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
doGet (ValueNum i) = do validSlot i
                        getProponentField i
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
doInc (ValueNum i) = do validSlot i
                        v <- getProponentVitality i
                        let v' = case v of
                              65535 -> 65535
                              0     -> 0
                              (-1)  -> (-1)
                              _     -> v+1
                        putProponentVitality v' i
                        return valueI
doInc (ValueCard ZeroCard) = doInc (ValueNum 0)
doInc _ = throwError incNANmsg
incNANmsg = "inc applied to non-number"

doDec :: Value -> MoveStep Value
doDec (ValueNum i) = if i >= 0 && i <= 255
                     then do v <- getOpponentVitality (255-i)
                             let v' = case v of
                                   0  -> 0
                                   -1 -> -1
                                   _  -> v-1
                             putOpponentVitality v' (255-i)
                             return $ ValueCard IdentityCard
                     else throwError decRangeMsg
doDec _ = throwError decNANmsg
decRangeMsg = "dec out of range"
decNANmsg = "dec applied to non-number"

doAttack :: Value -> Value -> Value -> MoveStep Value
doAttack (ValueNum i) arg2 (ValueNum n) =
  do validSlot i
     v <- getProponentVitality i
     if v < n
       then throwError attackRangeN
       else do putProponentVitality (v-n) i
               j <- case arg2 of
                 ValueNum jj -> return jj
                 _ -> throwError attackNANj
               validSlot j
               w <- getOpponentVitality (255-j)
               let n' = (n*9) `div` 10
                   w' = if      w <= 0  then w
                        else if w <= n' then 0
                             else            w-n'
               putOpponentVitality w' (255-j)
               return $ valueI
doAttack _ _ _ = throwError attackNAN

attackRangeI = "attack i-value out of range"
attackRangeN = "attack n-value greater than vitality of [i]"
attackRangeJ = "attack j-value out of range"
attackNANj = "attack j-value is a non-number (health still decremented)"
attackNAN = "attack i or n value is a non-number"

doHelp :: Value -> Value -> Value -> MoveStep Value
doHelp (ValueNum i) arg2 (ValueNum n) =
  do validSlot i
     v <- getProponentVitality i
     if v < n
       then throwError helpRangeN
       else do putProponentVitality (v-n) i
               j <- case arg2 of
                 ValueNum jj -> return jj
                 _ -> throwError helpNANj
               validSlot j
               w <- getProponentVitality j
               let n' = (n*11) `div` 10
                   w' = w+n'
                   w'' = if      w  <= 0     then w
                         else if w' >= 65535 then 65535
                              else                w'
               putProponentVitality w'' j
               return $ valueI

doHelp _ _ _ = throwError helpNAN

helpRangeI = "help i-value out of range"
helpRangeN = "help n-value greater than vitality of [i]"
helpRangeJ = "help j-value out of range"
helpNANj = "help j-value is a non-number (health still decremented)"
helpNAN = "help i or n value is a non-number"

doCopy :: Value -> MoveStep Value
doCopy (ValueNum i) = do validSlot i
                         getOpponentField i -- note that this is NOT (255-i)!
doCopy _ = throwError copyNANmsg
copyNANmsg = "copy applied to non-number"

doRevive :: Value -> MoveStep Value
doRevive (ValueNum i) = do validSlot i
                           v <- getProponentVitality i
                           let v' = case v of
                                 0  -> 1
                                 (-1) -> 1
                                 _  -> v
                           putProponentVitality v' i
                           return valueI
doRevive _ = throwError reviveNANmsg
reviveNANmsg = "revive applied to non-number"

doZombie :: Value -> Value -> MoveStep Value
doZombie (ValueNum i) x =
    do validSlot i
       v <- getOpponentVitality (255-i)
       if v > 0
         then throwError zombieNotDead
         else do putOpponentVitality (-1) (255-i)
                 putOpponentField x (255-i)
                 return $ valueI
zombieNotDead = "zombie called on cell that isn't dead"

test_CardBehavior = [
  runMove (doI (ValueNum 3)) initialState ~?=
    (initialState,Right $ ValueNum 3),
  runMove (doI valueI) initialState ~?=
    (initialState,Right $ valueI),

  runMove doZero initialState ~?= (initialState,Right $ ValueNum 0),

  runMove (doSucc (ValueNum 3)) initialState ~?=
    (initialState,Right $ ValueNum 4),
  runMove (doSucc (ValueNum 0)) initialState ~?=
    (initialState,Right $ ValueNum 1),
  runMove (doSucc (ValueNum 65534)) initialState ~?=
    (initialState,Right $ ValueNum 65535),
  runMove (doSucc (ValueNum 65535)) initialState ~?=
    (initialState,Right $ ValueNum 65535),
  runMove (doSucc valueI) initialState ~?=
    (initialState,Left succNANmsg),
  runMove (doSucc valueZero) initialState ~?=
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
  runMove (doDbl valueI) initialState ~?=
    (initialState,Left dblNANmsg),

  runMove (doGet (ValueNum 0)) initialState ~?=
    (initialState,Right $ valueI),
  runMove (doGet (ValueNum 255)) initialState ~?=
    (initialState,Right $ valueI),
  runMove (doGet (ValueNum 256)) initialState ~?=
    (initialState,Left invalidSlotError),
  runMove (doGet valueI) initialState ~?=
    (initialState,Left getNANmsg),

  runMove (doPut (ValueNum 345)) initialState ~?=
    (initialState,Right $ valueI),
  runMove (doPut valueS) initialState ~?=
    (initialState,Right $ valueI),

  runMove (doS (ValueNum 0) valueI valueI) initialState ~?=
    (initialState,Left applyNumMsg),
  runMove (doS valueI (ValueNum 0) valueI) initialState ~?=
    (initialState,Left applyNumMsg),
  runMove (doS valueI valueI (ValueNum 0)) initialState ~?=
    (initialState,Left applyNumMsg),
  runMove (doS valueI valueI valueI) initialState ~?=
    (initialState,Right valueI),
  -- Paul made this example
  runMove (doS (ValueApplication valueK valueSucc) valueSucc valueZero) initialState ~?=
    (initialState,Right $ ValueNum 2),
  runMove (apply (ValueApplication
                  (ValueApplication
                   valueS
                   (ValueApplication
                    valueK
                    valueSucc))
                  valueSucc)
           valueZero) initialState ~?=
    (initialState,Right $ ValueNum 2),

  runMove (doK (ValueNum 3) (ValueNum 6)) initialState ~?=
    (initialState,Right $ ValueNum 3),
  runMove (doK valueS valueSucc) initialState ~?=
    (initialState,Right valueS),

  runMove (doInc valueI) initialState ~?=
    (initialState,Left incNANmsg),
  runMove (doInc (ValueNum 256)) initialState ~?=
    (initialState,Left invalidSlotError),
  runMove (doInc (ValueNum 0)) initialState ~?=
    (alterFirstBoard (updateVitality 10001 0) initialState, Right valueI),
  runMove (doInc (ValueNum 0)) (alterFirstBoard (updateVitality 0 0) initialState) ~?=
    (alterFirstBoard (updateVitality 0 0) initialState, Right valueI),
  runMove (doInc (ValueNum 0)) (alterFirstBoard (updateVitality 65535 0) initialState) ~?=
    (alterFirstBoard (updateVitality 65535 0) initialState, Right valueI),
  runMove (doInc (ValueCard ZeroCard)) initialState ~?=
    (alterFirstBoard (updateVitality 10001 0) initialState, Right valueI)
  {-
  -- Infinite loop example
  runMove (doS (ValueCard IdentityCard) (ValueCard IdentityCard)
           (ValueCard SuccCard))
  initialState ~?=
    (initialState,Right $ ValueCard IdentityCard) -}
  ]
