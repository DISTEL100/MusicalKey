module MusicalKey.Tuning.Tuning (midiNote, MidiNote, Tuning (..), MidiTuning, FreqTuning, (!>), (!=)) where

import Data.Group (Group ((~~)))
import Data.Map (Map)
import Data.Map qualified as M
import MusicalKey.Interval (Frequency, Interval, (<>>))
import MusicalKey.Pitch (IsPitch (toPitch, deg, rep), Pitch, convertPitch)
import MusicalKey.TuningSystem (TuningSystem (..), degreesBetween)

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)

midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

data Tuning out
  = forall p ts. (IsPitch p, TuningSystem ts) => TunedByRef ts (Pitch p) out
  | forall ts. ( TuningSystem ts) => TunedByFunc ts (Interval -> out)
  | forall p . (IsPitch p) => TunedByMap (Map (Pitch p) out)
  | forall p. (IsPitch p) => TunedDirect (Pitch p -> out)

type MidiTuning = Tuning MidiNote

type FreqTuning = Tuning Frequency

(!>) :: (IsPitch b) => FreqTuning -> Pitch b -> Frequency
TunedDirect tFunc !> pitch = tFunc $ convertPitch pitch
TunedByMap mapping !> pitch = mapping M.! convertPitch pitch
TunedByFunc tSystem tFunc !> pitch = tFunc $ tSystem !% pitch
TunedByRef tSystem refPitch refC !> pitch =
  let interval = tSystem !% pitch :: Interval
      refInterval = tSystem !% refPitch :: Interval
   in refC <>> (interval ~~ refInterval)

(!=) ::(IsPitch b)=> MidiTuning -> Pitch b -> MidiNote
TunedDirect tFunc != pitch = tFunc $ convertPitch pitch
TunedByMap mapping != pitch = mapping M.! convertPitch pitch
TunedByFunc tSystem tFunc != pitch = tFunc $ tSystem !% pitch
TunedByRef tSystem refPitch refMidiNote@(MidiNote m) != pitch
  | refPitch == convertPitch pitch = refMidiNote
  | otherwise = MidiNote $ m + degreesBetween tSystem refPitch (convertPitch pitch)
