module MusicalKey.Tuning (module MusicalKey.Tuning) where

import Data.Group (Group (pow), invert, (~~))
import Data.Set qualified as Set
import MusicalKey.Interval

type Degree = Int
type Repeat = Int
type Pitch = (Degree, Repeat)

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)
midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 127"
  | a < 0 = error "a MidiNote must be larger then 0"
  | otherwise = MidiNote a

class TuningSystem a b where
  (!%!) :: a -> b -> Interval

class Tuning a b where
  (!>!) :: a -> b -> Frequency

class MidiTuning a b where
  (!=!) :: a -> b -> MidiNote

instance TuningSystem (Set.Set Interval) Degree where
  (!%!) set deg
    | Set.null set = mempty
    | deg == 0 = Set.elemAt 0 set ~~ Set.elemAt 0 set
    | deg > 0 =
        let dm = divMod (pred deg) $ Set.size set
         in Set.elemAt (snd dm) set <> pow (Set.findMax set) (fst dm)
    | otherwise = invert $ set !%! (-deg)

instance TuningSystem (Set.Set Interval) Pitch where
  (!%!) a (0, 0) = Set.elemAt 0 a ~~ Set.elemAt 0 a
  (!%!) a (0, rep) = pow (Set.findMax a) rep
  (!%!) a (deg, rep) = a !%! deg <> a !%! (0 :: Degree, rep)

instance TuningSystem [Interval] Degree where
  (!%!) = (!%!) . Set.fromList

instance TuningSystem [Interval] Pitch where
  (!%!) = (!%!) . Set.fromList

data TunedByRef a b where
  TunedByRef :: (TuningSystem a b) => {tuningSystem :: a, reference :: b, refFreq :: Frequency} -> TunedByRef a b

instance Tuning (TunedByRef a b) b where
  TunedByRef {tuningSystem = ts, reference = ref, refFreq = refF} !>! b = tuneByRef ts ref refF b

tuneByRef :: (TuningSystem a b) => a -> b -> Frequency -> b -> Frequency
tuneByRef a refB refF b =
  let interval = a !%! b :: Interval
      refInterval = a !%! refB :: Interval
   in refF <<> refInterval <>> interval

midiTuneByRef :: (Enum a) => a -> MidiNote -> a -> MidiNote
midiTuneByRef refB (MidiNote refMidiNote) b = MidiNote $ (fromEnum b - fromEnum refB) + refMidiNote

instance MidiTuning (MidiTuningByRef a Degree) Degree where
 MidiTuningByRef {tuning=t, refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b
 MidiTuningSystemByRef {tuningSys=ts, refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b

instance MidiTuning (MidiTuningByRef a Pitch) Pitch where
 MidiTuningByRef {tuning=t, refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b
 MidiTuningSystemByRef {tuningSys=ts, refB=refB, refMidiNote=refMidiNote} !=! b = midiTuneByRef refB refMidiNote b

data MidiTuningByRef a b where
  MidiTuningByRef :: (Tuning a b) => {tuning :: a, refB :: b, refMidiNote :: MidiNote} -> MidiTuningByRef a b
  MidiTuningSystemByRef :: (TuningSystem a b) => {tuningSys :: a, refB :: b, refMidiNote :: MidiNote} -> MidiTuningByRef a b
