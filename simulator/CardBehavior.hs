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
nanMsg = "Something applied to a non-number"

doI :: Value -> MoveStep Value
doI x = return x

doZero :: MoveStep Value
doZero = return $ ValueNum 0

doSucc :: Value -> MoveStep Value
doSucc (ValueNum n) = return $ makeNumVal $ n+1
doSucc (ValueCard ZeroCard) = doSucc (ValueNum 0)
doSucc _ = throwError nanMsg

doDbl :: Value -> MoveStep Value
doDbl (ValueNum n) = return $ makeNumVal $ n * 2
doDbl (ValueCard ZeroCard) = doDbl (ValueNum 0)
doDbl _  = throwError nanMsg

doGet :: Value -> MoveStep Value
doGet (ValueNum i) = do validSlot i
                        p <- myFriend
                        v <- getVitality p i
                        if v <= 0
                          then throwError getFromDead
                          else getField p i
doGet (ValueCard ZeroCard) = doGet (ValueNum 0)
doGet _ = throwError nanMsg
getFromDead = "tried to get from a dead cell"

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
                        p <- myFriend
                        applyVitalityConsequence p i 1
                        return valueI
doInc (ValueCard ZeroCard) = doInc (ValueNum 0)
doInc _ = throwError nanMsg

doDec :: Value -> MoveStep Value
doDec (ValueNum i) = do validSlot i
                        p <- myEnemy
                        applyVitalityConsequence p (255-i) (-1)
                        return valueI
doDec (ValueCard ZeroCard) = doDec (ValueNum 0)
doDec _ = throwError nanMsg

doAttack :: Value -> Value -> Value -> MoveStep Value
doAttack (ValueNum i) arg2 (ValueNum n) =
  do validSlot i
     f <- myFriend
     e <- myEnemy
     v <- getVitality f i
     if v < n
       then throwError attackRangeN
       else do payVitalityCost f i n
               j <- case arg2 of
                 ValueNum jj -> return jj
                 ValueCard ZeroCard -> return 0
                 _ -> throwError attackNANj
               validSlot j
               let ed = (n*9) `div` 10
               applyVitalityConsequence e (255-j) (-ed)
               return $ valueI
doAttack (ValueCard ZeroCard) b c = doAttack (ValueNum 0) b c
doAttack a b (ValueCard ZeroCard) = doAttack a b (ValueNum 0)
doAttack _ _ _ = throwError nanMsg

attackRangeN = "attack n-value greater than vitality of [i]"
attackNANj = "attack j-value is a non-number (health still decremented)"

doHelp :: Value -> Value -> Value -> MoveStep Value
doHelp (ValueNum i) arg2 (ValueNum n) =
  do validSlot i
     p <- myFriend
     v <- getVitality p i
     if v < n
       then throwError helpRangeN
       else do payVitalityCost p i n
               j <- case arg2 of
                 ValueNum jj -> return jj
                 ValueCard ZeroCard -> return 0
                 _ -> throwError helpNANj
               validSlot j
               let h = (n*11) `div` 10
               applyVitalityConsequence p j h
               return $ valueI
doHelp (ValueCard ZeroCard) b c = doHelp (ValueNum 0) b c
doHelp a b (ValueCard ZeroCard) = doHelp a b (ValueNum 0)
doHelp _ _ _ = throwError nanMsg

helpRangeN = "help n-value greater than vitality of [i]"
helpNANj = "help j-value is a non-number (health still decremented)"

doCopy :: Value -> MoveStep Value
doCopy (ValueNum i) = do validSlot i
                         p <- myEnemy
                         getField p i -- note that this is NOT (255-i)!
doCopy (ValueCard ZeroCard) = doCopy (ValueNum 0)
doCopy _ = throwError nanMsg

doRevive :: Value -> MoveStep Value
doRevive (ValueNum i) = do validSlot i
                           p <- myFriend
                           setVitalityOnDeadSlot p i 1
                           return valueI
doRevive (ValueCard ZeroCard) = doRevive (ValueNum 0)
doRevive _ = throwError nanMsg

doZombie :: Value -> Value -> MoveStep Value
doZombie (ValueNum i') x =
    do let i = 255-i'
       validSlot i
       p <- myEnemy
       v <- getVitality p i
       if v > 0
         then throwError zombieNotDead
         else do setVitalityOnDeadSlot p i (-1)
                 setField p i x
                 return $ valueI
doZombie (ValueCard ZeroCard) x = doZombie (ValueNum 0) x
doZombie _ _ = throwError nanMsg
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
    (initialState,Left nanMsg),
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
    (initialState,Left nanMsg),

  runMove (doGet (ValueNum 0)) initialState ~?=
    (initialState,Right $ valueI),
  runMove (doGet (ValueNum 255)) initialState ~?=
    (initialState,Right $ valueI),
  runMove (doGet (ValueNum 256)) initialState ~?=
    (initialState,Left invalidSlotError),
  runMove (doGet valueI) initialState ~?=
    (initialState,Left nanMsg),

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
    (initialState,Left nanMsg),
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
  ]
  where
    zombieState = beginZombieApocolypse initialState
