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

data TunedByReference a b where
  ByRef :: (TuningSystem a b) => {tuningSystem :: a, reference :: b, referenceFreq :: Frequency} -> TunedByReference a b

instance Tuning (TunedByReference a b) b where
  ByRef{tuningSystem = ts, reference = ref, referenceFreq = refF} !>! b =
    let interval = ts !%! b :: Interval
        refInterval = ts !%! ref :: Interval
     in refF <<> refInterval <>> interval
