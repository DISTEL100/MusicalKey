module MusicalKey.TuningSystem (module MusicalKey.TuningSystem) where

import Data.Group (Group (pow), invert, (~~))
import Data.Set qualified as Set
import MusicalKey.Interval

type Degree = Int
degr :: Int -> Degree
degr i = i

type Repeat = Int

type Pitch = (Degree, Repeat)
pitch :: Int -> Int -> Pitch
pitch deg rep = (deg, rep)

pitchToDegree :: Pitch -> Int -> Degree
pitchToDegree (deg, rep) repeatSize = repeatSize * rep + deg

class TuningSystem a b where
  (!%!) :: a -> b -> Interval

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

