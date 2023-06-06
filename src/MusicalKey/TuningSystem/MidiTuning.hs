module MusicalKey.TuningSystem.MidiTuning where

import MusicalKey.TuningSystem
import MusicalKey.TuningSystem.Tuning
import MusicalKey.Interval
import qualified Data.Set as Set
import qualified Data.Set as Set
import MusicalKey.Pitch 

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)
midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

data MidiTuning a b
  = (TuningSystem a (Pitch b)) => TunedByRef a (Pitch b) MidiNote
  | (TuningSystem a (Pitch b)) => TunedByFunc a (Interval -> MidiNote)
  | (TuningSystem a (Pitch b)) => TunedByMap (M.Map (Pitch b) MidiNote)

midiTuneByRef :: (Enum a) => a -> MidiNote -> a -> MidiNote
midiTuneByRef refB (MidiNote refMidiNote) b = MidiNote $ (fromEnum b - fromEnum refB) + refMidiNote

instance (IsPitch b) => MidiTuning (MidiTuningByRef a b) b where
 MidiTuningByRef {refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b
 MidiTuningSystemByRef {refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b
