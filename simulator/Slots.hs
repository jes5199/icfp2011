module Slots (Vitality, Slot(..), Slots(..),
              extractVitality, extractField,
              updateVitality, updateField,
              changeVitalityInSlot, replaceVitalityOnDeadSlot,
              initialSide, test_Slots) where

import Test.HUnit
import Data.Array
import Card
import Value
import Move
import Util

type Vitality = Int

data Slot = Slot { vitality :: !Vitality, field :: !Value }
          deriving (Eq)

instance Show Slot where
    show (Slot v f) = "{" ++ (show v) ++ "," ++ (show f) ++ "}"

replaceVitality :: Vitality -> Slot -> Slot
replaceVitality hp slot = Slot hp (field slot)

changeVitality :: Vitality -> Slot -> Slot
changeVitality hp slot = Slot (clampInt 0 65535
                               (addIfPositive (vitality slot) hp)) (field slot)

replaceVitalityIfDead :: Vitality -> Slot -> Slot
replaceVitalityIfDead hp slot = if (vitality slot) > 0 then slot else Slot hp (field slot)

replaceField :: Value -> Slot -> Slot
replaceField value slot = Slot (vitality slot) value

newtype Slots = Slots (Array Int Slot)
    deriving Eq

instance Show Slots where
    show (Slots arr) = concatMap showSlotOnALine $ filter isInteresting $ assocs arr
        where isInteresting (_, Slot 10000 (ValueCard IdentityCard)) = False
              isInteresting _ = True
              showSlotOnALine (n, slot) = (show n) ++ "=" ++ (show slot) ++ "\n"

transformSlot :: (Slot -> Slot) -> SlotNumber -> Slots -> Slots
transformSlot transformation idx (Slots slots) = Slots $ slots // [(idx, transformation (slots ! idx) )]

updateVitality :: Vitality -> SlotNumber -> Slots -> Slots
updateVitality hp idx slots = transformSlot (replaceVitality hp) idx slots

changeVitalityInSlot :: Slots -> SlotNumber -> Vitality -> Slots
changeVitalityInSlot slots idx adjustment = transformSlot (changeVitality adjustment) idx slots

replaceVitalityOnDeadSlot :: Slots -> SlotNumber -> Vitality -> Slots
replaceVitalityOnDeadSlot slots idx vitality = transformSlot (replaceVitalityIfDead vitality) idx slots

updateField :: Value -> SlotNumber -> Slots -> Slots
updateField value idx slots = transformSlot (replaceField value) idx slots

extractVitality :: Slots -> SlotNumber -> Vitality
extractVitality (Slots slots) idx = vitality (slots ! idx)

extractField :: Slots -> SlotNumber -> Value
extractField (Slots slots) idx = field (slots ! idx)

initialSide :: Slots
initialSide = Slots $
              array (0,255::Int) [(n,Slot 10000 (valueI)) |
                                  n <- [0..255]]

test_Slots = [
    updateVitality 19 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 19 valueI)]),
    updateField (ValueNum 5) 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 10000 (ValueNum 5))])
    ]
  where
    testSlots = array (0,3::Int) [(n,Slot 10000 (valueI)) |
                        n <- [0..3]]
