module MusicalKey.TuningSystem.Tuning where

import MusicalKey.TuningSystem
import MusicalKey.Interval

class Tuning a b where
  (!>!) :: a -> b -> Frequency

data TunedByRef a b where
  TunedByRef :: (TuningSystem a b) => {tuningSystem :: a, reference :: b, refFreq :: Frequency} -> TunedByRef a b

instance Tuning (TunedByRef a b) b where
  TunedByRef {tuningSystem = ts, reference = ref, refFreq = refF} !>! b = tuneByRef ts ref refF b

tuneByRef :: (TuningSystem a b) => a -> b -> Frequency -> b -> Frequency
tuneByRef a refB refF b =
  let interval = a !%! b :: Interval
      refInterval = a !%! refB :: Interval
   in refF <<> refInterval <>> interval

