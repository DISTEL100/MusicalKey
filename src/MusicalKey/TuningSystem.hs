module MusicalKey.TuningSystem (module MusicalKey.TuningSystem) where

import Data.Data
import Data.Group (Group (pow), invert)
import Data.List (sort)
import Data.Set qualified as Set
import MusicalKey.Interval (Frequency (Freq), Interval (Cent, Ratio), (<>>))
import MusicalKey.Pitch

-- TuningSystem
class TuningSystem sys pitch where
  (!%) :: Proxy sys -> pitch -> Interval

indexSetByDegree :: Set.Set Interval -> Degree -> Interval
indexSetByDegree set pitch =
  let setSize = Set.size set
      index = pitch `mod` setSize
   in Set.elemAt index set

-- Todo negative degree/octave
indexSetByWPitch :: Set.Set Interval -> WPitch -> Interval
indexSetByWPitch set (deg, oct) =
  let setSize = Set.size set
      (degMod, degDiv) = deg `divMod` setSize
      octaveInterval = Set.findMax set
      intervalDeg = Set.elemAt degMod set
   in intervalDeg <> pow octaveInterval (oct + degDiv)

sortedCentSet :: [Double] -> Set.Set Interval
sortedCentSet cents = Set.fromList $ map Cent (sort cents)

ratioSet :: [Rational] -> Set.Set Interval
ratioSet ratios = Set.fromList $ map Ratio (sort ratios)

-- Example TuningSystem Equal temperament
data ET12 = ET12

et12Intervals :: Set.Set Interval
et12Intervals = sortedCentSet [100, 200 .. 1200]

instance TuningSystem ET12 WPitch where
  _ !% pitch = indexSetByWPitch et12Intervals pitch
