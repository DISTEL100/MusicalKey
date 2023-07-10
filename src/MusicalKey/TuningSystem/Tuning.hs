module MusicalKey.TuningSystem.Tuning (midiNote, MidiNote, Tuning (..), MidiTuning, FreqTuning, StdFreqTuning, StdMidiTuning, (!>), (!=)) where

import Data.Group (Group ((~~)))
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set qualified as Set
import MusicalKey.Interval (Frequency, Interval, (<>>))
import MusicalKey.Pitch (IsPitch, Pitch)
import MusicalKey.TuningSystem (TuningSystem (..), degreesBetween)

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)
midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

data Tuning a b c
  = (IsPitch b, TuningSystem a b) => TunedByRef a (Pitch b) c
  | (IsPitch b, TuningSystem a b) => TunedByFunc a (Interval -> c)
  | (IsPitch b, TuningSystem a b) => TunedByMap (Map (Pitch b) c)
  | TunedDirect (Pitch b -> c)

type MidiTuning a b = Tuning a b MidiNote
type FreqTuning a b = Tuning a b Frequency
type StdFreqTuning = FreqTuning (Set.Set Interval) (Int, Int)
type StdMidiTuning = MidiTuning (Set.Set Interval) (Int, Int)

(!>) :: FreqTuning a b -> Pitch b -> Frequency
TunedDirect tFunc !> pitch = tFunc pitch
TunedByMap mapping !> pitch = mapping M.! pitch
TunedByFunc tSystem tFunc !> pitch = tFunc $ tSystem !% pitch
TunedByRef tSystem refPitch refC !> pitch =
  let interval = tSystem !% pitch :: Interval
      refInterval = tSystem !% refPitch :: Interval
   in refC <>> (interval ~~ refInterval)

(!=) :: MidiTuning a b -> Pitch b -> MidiNote
TunedDirect tFunc != pitch = tFunc pitch
TunedByMap mapping != pitch = mapping M.! pitch
TunedByFunc tSystem tFunc != pitch = tFunc $ tSystem !% pitch
TunedByRef tSystem refPitch refMidiNote@(MidiNote m) != pitch
  | refPitch == pitch = refMidiNote
  | otherwise = MidiNote $ m + degreesBetween tSystem refPitch pitch
