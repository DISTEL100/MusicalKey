module MusicalKey.TuningSystem (module MusicalKey.TuningSystem) where

import Data.Group (Group (pow), invert, (~~))
import Data.Map qualified as M
import Data.Set qualified as Set
import MusicalKey.Interval
import MusicalKey.Pitch (IsPitch (deg, rep))

class IsPitch b => TuningSystem a b where
  ( !%!) :: a -> b -> Interval

instance (IsPitch b) => TuningSystem (Set.Set Interval) b where
  set !%! p
    | Set.null set = mempty
    | deg p == 0 = memptyOf $ Set.elemAt 0 set
    | deg p > 0 =
        let dm = divMod (pred (deg p)) $ Set.size set
         in Set.elemAt (snd dm) set <> pow (Set.findMax set) (fst dm + rep p)
    | otherwise = invert $ set !%! (-(deg p))

instance (IsPitch a) => TuningSystem [Interval] a where
  (!%!) = (!%!) . Set.fromList

instance (Ord b, IsPitch b) => TuningSystem (M.Map b Interval) b where
  freqMap !%! b = freqMap M.! b

data EqDivTuning = EqDivTuning {divisions :: Int, repInterval :: Interval}
data PythagoreanTuning = PythagoreanTuning {generator :: Interval, size :: Int, foldInterval :: Interval}
-- data PrimeLimitTuning =
