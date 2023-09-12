module MusicalKey.TuningSystem (module MusicalKey.TuningSystem) where

import Data.Group (Group (pow), invert )
import Data.Map qualified as M
import Data.Set qualified as Set
import MusicalKey.Interval ( Interval )
import MusicalKey.Pitch (IsPitch (deg, rep, toPitch), Pitch (Pitch))

class TuningSystem a where
  (!%) :: forall b. (IsPitch b) => a -> Pitch b -> Interval

instance TuningSystem (Set.Set Interval) where
  set !% pitch
    | Set.null set = mempty
    | deg pitch <= 0 =
        let dm = divMod (pred (deg pitch)) $ Set.size set
         in Set.elemAt (snd dm) set <> pow (Set.findMax set) (fst dm + rep pitch)
    | otherwise = invert $ set !% invert pitch

instance TuningSystem [Interval] where
  (!%) = (!%) . Set.fromList

instance (IsPitch b) => TuningSystem (M.Map (Pitch b) Interval)  where
  freqMap !% p = freqMap M.! toPitch (deg p) (rep p)

degreesBetween :: forall a b . (IsPitch b, TuningSystem a ) => a -> Pitch b -> Pitch b -> Int
degreesBetween ts p1 p2 = degreesBetween' ts p1 p2 0

degreesBetween' :: forall a b . (IsPitch b, TuningSystem a) => a -> Pitch b -> Pitch b -> Int -> Int
degreesBetween' ts p1 p2 acc
  | ts !% p1 == ts !% p2 = acc
  | ts !% p1 < ts !% p2 = degreesBetween' ts (toPitch (deg p1 + 1) (rep p1)) p2 (acc + 1)
  | otherwise = -(degreesBetween' ts p2 p1 acc)
