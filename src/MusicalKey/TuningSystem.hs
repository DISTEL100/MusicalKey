module MusicalKey.TuningSystem (module MusicalKey.TuningSystem) where

import Data.Group (Group (pow), invert )
import Data.Map qualified as M
import Data.Set qualified as Set
import MusicalKey.Interval ( Interval )
import MusicalKey.Pitch (IsPitch (deg, rep, toPitch), Pitch (Pitch))

class TuningSystem a b where
  (!%) :: a -> Pitch b -> Interval

instance IsPitch b => TuningSystem (Set.Set Interval) b where
  set !% pitch
    | Set.null set = mempty
    | deg pitch <= 0 =
        let dm = divMod (pred (deg pitch)) $ Set.size set
         in Set.elemAt (snd dm) set <> pow (Set.findMax set) (fst dm + rep pitch)
    | otherwise = invert $ set !% invert pitch

instance IsPitch b => TuningSystem [Interval] b where
  (!%) = (!%) . Set.fromList

instance (Ord b) => TuningSystem (M.Map b Interval) b where
  freqMap !% (Pitch b) = freqMap M.! b

degreesBetween :: (IsPitch b, TuningSystem a b) => a -> Pitch b -> Pitch b -> Int
degreesBetween ts p1 p2 = degreesBetween' ts p1 p2 0

degreesBetween' :: (IsPitch b, TuningSystem a b) => a -> Pitch b -> Pitch b -> Int -> Int
degreesBetween' ts p1 p2 acc
  | ts !% p1 == ts !% p2 = acc
  | ts !% p1 < ts !% p2 = degreesBetween' ts (toPitch (deg p1 + 1) (rep p1)) p2 (acc + 1)
  | otherwise = -(degreesBetween' ts p2 p1 acc)
