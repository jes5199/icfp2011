module Slots (Vitality, Slot, vitality, field,
              SlotNumber, Slots(..), initialSide,
              extractVitality, extractField,
              updateVitality, updateField,
              changeVitalityInSlot, replaceVitalityOnDeadSlot,
              test_Slots) where

import Test.HUnit
import Data.Array
import Card
import Value
import Util

type Vitality = Int

data Slot = Slot { vitality :: !Vitality, field :: !Value }
          deriving Eq

instance Show Slot where
    show (Slot v f) = "{" ++ (show v) ++ "," ++ (show f) ++ "}"

-- These are private
initialSlot = Slot { vitality = 10000, field = valueI }

slotReplaceVitality :: Vitality -> Slot -> Slot
slotReplaceVitality hp slot = slot { vitality = hp }

slotReplaceField :: Value -> Slot -> Slot
slotReplaceField value slot = slot { field = value }

slotChangeVitality :: Vitality -> Slot -> Slot
slotChangeVitality hp slot = slotReplaceVitality
                             (clampInt 0 65535
                              (addIfPositive (vitality slot) hp))
                             slot

slotReplaceVitalityIfDead :: Vitality -> Slot -> Slot
slotReplaceVitalityIfDead hp slot = if vitality slot > 0
                                    then slot
                                    else slotReplaceVitality hp slot
-- end private

type SlotNumber = Int

newtype Slots = Slots (Array SlotNumber Slot)
              deriving Eq

instance Show Slots where
  show (Slots arr) = concatMap showSlotOnALine $
                     filter isInteresting $
                     assocs arr
    where isInteresting = (/= initialSlot) . snd
          showSlotOnALine (n, slot) = (show n) ++ "=" ++ (show slot) ++ "\n"

initialSide :: Slots
initialSide = Slots $ array (0,255) [(n,initialSlot) | n <- [0..255]]

transformSlot :: (Slot -> Slot) -> SlotNumber -> Slots -> Slots
transformSlot transformation idx (Slots slots) =
  Slots $ slots // [(idx, transformation (slots ! idx) )]

extractVitality :: Slots -> SlotNumber -> Vitality
extractVitality (Slots slots) idx = vitality (slots ! idx)

extractField :: Slots -> SlotNumber -> Value
extractField (Slots slots) idx = field (slots ! idx)

updateVitality :: Vitality -> SlotNumber -> Slots -> Slots
updateVitality hp idx slots = transformSlot (slotReplaceVitality hp) idx slots

updateField :: Value -> SlotNumber -> Slots -> Slots
updateField value idx slots = transformSlot (slotReplaceField value) idx slots

changeVitalityInSlot :: SlotNumber -> Vitality -> Slots -> Slots
changeVitalityInSlot idx adjustment slots =
  transformSlot (slotChangeVitality adjustment) idx slots

replaceVitalityOnDeadSlot :: SlotNumber -> Vitality -> Slots -> Slots
replaceVitalityOnDeadSlot idx vitality slots =
  transformSlot (slotReplaceVitalityIfDead vitality) idx slots

test_Slots = [
    updateVitality 19 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 19 valueI)]),
    updateField (ValueNum 5) 1 (Slots testSlots) ~?= Slots (testSlots // [(1, Slot 10000 (ValueNum 5))])
    ]
  where
    testSlots = array (0,3::Int) [(n,Slot 10000 (valueI)) |
                        n <- [0..3]]
