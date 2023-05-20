module Tuning where

import           Data.Group          (invert)
import qualified Data.Set            as Set
import           MusicalKey.Interval (Frequency, Interval, (%>%))

type Degree= Int

data TuningSystem a = TuningSystem { intervals::(Interval a)=> Set.Set a, equivalentInterval :: (Interval a) => a }

getInterval :: (Interval a) => TuningSystem a -> Degree -> a
(getInterval) (TuningSystem set equi) deg =
  let size = Set.size set :: Int
      index = deg `mod` size :: Int
      eqs = deg `div` size :: Int
      interval = Set.elemAt index set
      eqInterval = intervalPower equi eqs
    in if deg >= 0 then eqInterval else invert eqInterval

intervalPower :: (Interval a) => a -> Int -> a
intervalPower interval power = let iterated = (iterate (mappend interval) mempty) !! power
                               in if power >= 0 then iterated else invert iterated

data Tuning a = Tuning { system::TuningSystem a, referenceFreq:: Frequency, referenceDegree:: (Degree, Int) }

freqOfDegree :: (Interval a) => Tuning a -> (Degree, Int) -> Frequency
freqOfDegree (Tuning ts@(TuningSystem intSet eqInt) refFreq (refDeg, refEq)) (deg, eq) =
  let diffEqs = refEq - eq
      eqInterval = intervalPower eqInt diffEqs
      degInterval = (invert  $ getInterval ts refDeg) `mappend` (getInterval ts deg)
    in refFreq %>% (mappend eqInterval degInterval)
