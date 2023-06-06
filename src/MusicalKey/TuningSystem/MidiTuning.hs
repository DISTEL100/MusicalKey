module MusicalKey.TuningSystem.MidiTuning where

import MusicalKey.TuningSystem
import MusicalKey.TuningSystem.Tuning
import MusicalKey.Interval
import qualified Data.Set as Set

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)
midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

class MidiTuning a b where
  (!=!) :: a -> b -> MidiNote

data MidiTuningByRef a b where
  MidiTuningByRef :: (Tuning a b) => {tuning :: a, refB :: b, refMidiNote :: MidiNote} -> MidiTuningByRef a b
  MidiTuningSystemByRef :: (TuningSystem a b) => {tuning :: a, refB :: b, refMidiNote :: MidiNote} -> MidiTuningByRef a b

midiTuneByRef :: (Enum a) => a -> MidiNote -> a -> MidiNote
midiTuneByRef refB (MidiNote refMidiNote) b = MidiNote $ (fromEnum b - fromEnum refB) + refMidiNote

instance MidiTuning (MidiTuningByRef a Degree) Degree where
 MidiTuningByRef {refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b
 MidiTuningSystemByRef {refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b

instance MidiTuning (MidiTuningByRef (Set.Set Interval) Pitch) Pitch where
 midiT !=! b = 
  let repeatSize = Set.size $ tuning midiT
      refBDeg = pitchToDegree (refB midiT) repeatSize :: Degree
      bDeg = pitchToDegree b repeatSize :: Degree
  in midiTuneByRef refBDeg (refMidiNote midiT) bDeg
