module MusicalKey.TuningSystem.Tuning where

import MusicalKey.TuningSystem
import MusicalKey.Interval
import qualified Data.Map as M
import MusicalKey.Pitch (IsPitch)

class IsPitch b => Tuning a b where
  (!>!) :: a -> b -> Frequency

data  TunedByRef a b where
  TunedByRef :: (IsPitch b, TuningSystem a b) => {system :: a, reference :: b, refFreq :: Frequency} -> TunedByRef a b

instance (IsPitch b) => Tuning (TunedByRef a b) b where
  TunedByRef {system = ts, reference = ref, refFreq = refF} !>! b = tuneByRef ts ref refF b

tuneByRef :: (TuningSystem a b) => a -> b -> Frequency -> b -> Frequency
tuneByRef a refB refF b =
  let interval = a !%! b :: Interval
      refInterval = a !%! refB :: Interval
   in refF <<> refInterval <>> interval

instance (IsPitch b, Ord b) => Tuning (M.Map b Frequency) b where
  freqMap !>! b = freqMap M.! b

