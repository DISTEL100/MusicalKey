module MusicalKey.Tuning.Tuning (module MusicalKey.Tuning.Tuning) where

import Data.Group (Group ((~~)))
import MusicalKey.Interval (Frequency, Interval, (<>>))
import MusicalKey.TuningSystem (TuningSystem (..), degreesBetween)

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)

midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

class Tuning sys pitch out  where
  (!>!) :: sys -> pitch -> out

data TunedByRef ts refP refOut = TunedByRef ts refP refOut

instance (TuningSystem ts refP) =>
  Tuning (TunedByRef ts refP Frequency) refP Frequency where
    TunedByRef tSystem refPitch refC !>! pitch =
     let interval = tSystem !% pitch :: Interval
         refInterval = tSystem !% refPitch :: Interval
      in refC <>> (interval ~~ refInterval)

