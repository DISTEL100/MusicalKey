module MusicalKey.Tuning (module MusicalKey.Tuning) where

import Data.Group (Group (pow), invert, (~~))
import Data.Set qualified as Set
import MusicalKey.Interval

type Degree = Int
type Repeat = Int
type Pitch = (Degree, Repeat)
newtype MidiNote = MidiNote Int
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
  (!%!) set deg =
    let normalizedSet = Set.insert mempty set
     in Set.elemAt (deg `mod` Set.size normalizedSet) normalizedSet

instance TuningSystem (Set.Set Interval) Pitch where
  (!%!) a (deg, rep) =
    let normalizedSet = Set.insert mempty a
        equivalentInterval = Set.findMax normalizedSet
        repeats = rep + (deg `div` Set.size normalizedSet)
     in a !%! deg <> pow equivalentInterval repeats

instance TuningSystem [Interval] Degree where
  (!%!) = (!%!) . Set.fromList

instance TuningSystem [Interval] Pitch where
  (!%!) = (!%!) . Set.fromList

data TunedByReference a b where
  ByRef :: TuningSystem a b => {tuningSystem :: a, reference :: b, referenceFreq :: Frequency} -> TunedByReference a b

instance Tuning (TunedByReference a b) b where
  ByRef{tuningSystem = ts, reference = ref, referenceFreq = refF} !>! b =
    let interval = ts !%! b :: Interval
        refInterval = ts !%! ref :: Interval
     in refF <<> refInterval <>> interval
