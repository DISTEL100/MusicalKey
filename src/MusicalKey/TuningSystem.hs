module MusicalKey.TuningSystem (module MusicalKey.TuningSystem) where

import Data.Group (Group (pow), invert, (~~))
import Data.Map qualified as M
import Data.Set qualified as Set
import MusicalKey.Interval
import MusicalKey.Pitch (IsPitch (deg, rep), Pitch (Pitch))

class IsPitch b => TuningSystem a b where
  (!%!) :: a -> Pitch b -> Interval

instance IsPitch b => TuningSystem (Set.Set Interval) (b) where
  set !%! pitch@(Pitch p)
    | Set.null set = mempty
    | deg p == 0 = memptyOf $ Set.elemAt 0 set
    | deg p > 0 =
        let dm = divMod (pred (deg p)) $ Set.size set
         in Set.elemAt (snd dm) set <> pow (Set.findMax set) (fst dm + rep p)
    | otherwise = invert $ set !%! (invert pitch)

instance IsPitch b => TuningSystem [Interval] (b) where
  (!%!) = (!%!) . Set.fromList

instance (Ord b, IsPitch b) => TuningSystem (M.Map (b) Interval) (b) where
  freqMap !%! (Pitch b) = freqMap M.! b

data EqDivTuning = EqDivTuning {divisions :: Int, repInterval :: Interval}
data PythagoreanTuning = PythagoreanTuning {generator :: Interval, size :: Int, foldInterval :: Interval}
-- data PrimeLimitTuning =
