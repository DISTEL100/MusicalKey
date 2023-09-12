module MusicalKey.Tuning.Tuning (midiNote, MidiNote, (!>!), TunedByFunc (TunedByFunc), TunedByMap (TunedByMap), TunedByRef (TunedByRef), TunedDirect (TunedDirect), Tuning) where

import Data.Group (Group ((~~)))
import Data.Map (Map)
import Data.Map qualified as M
import MusicalKey.Interval (Frequency, Interval, (<>>))
import MusicalKey.Pitch (IsPitch, Pitch, convertPitch)
import MusicalKey.TuningSystem (TuningSystem (..), degreesBetween)

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)

midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

class Tuning a out where
  (!>!) :: forall p. (IsPitch p) => a -> Pitch p -> out

data TunedByRef out = forall p ts. (IsPitch p, TuningSystem ts) => TunedByRef ts (Pitch p) out

instance Tuning (TunedByRef Frequency) Frequency where
  TunedByRef tSystem refPitch refC !>! pitch =
    let interval = tSystem !% pitch :: Interval
        refInterval = tSystem !% refPitch :: Interval
     in refC <>> (interval ~~ refInterval)

instance Tuning (TunedByRef MidiNote) MidiNote where
  TunedByRef tSystem refPitch refMidiNote@(MidiNote m) !>! pitch
    | refPitch == convertPitch pitch = refMidiNote
    | otherwise = MidiNote $ m + degreesBetween tSystem refPitch (convertPitch pitch)

data TunedByFunc out = forall ts. (TuningSystem ts) => TunedByFunc ts (Interval -> out)

instance Tuning (TunedByFunc out) out where
  TunedByFunc tSystem tFunc !>! pitch = tFunc $ tSystem !% pitch

data TunedByMap out = forall p. (IsPitch p) => TunedByMap (Map (Pitch p) out)

instance Tuning (TunedByMap out) out where
  TunedByMap mapping !>! pitch = mapping M.! convertPitch pitch

data TunedDirect out = forall p. (IsPitch p) => TunedDirect (Pitch p -> out)

instance Tuning (TunedDirect out) out where
  TunedDirect tFunc !>! pitch = tFunc $ convertPitch pitch
