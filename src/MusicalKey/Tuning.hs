module Tuning where

import           Data.Group          (invert)
import qualified Data.Set            as Set
import           MusicalKey.Interval (Frequency, Interval, (%>%), (%+%), iterateInterval)

type Degree= Int

data TuningSystem a = TuningSystem { intervals::(Interval a)=> Set.Set a, equivalentInterval :: (Interval a) => a }

class DegreeIndexable a b where 
  (!!!) :: a -> Degree -> b

instance (Interval b) => DegreeIndexable (TuningSystem b) b where
 (!!!) (TuningSystem set equi) deg =
  let size = Set.size set :: Int
      index = deg `mod` size :: Int
      eqs = deg `div` size :: Int
      interval = Set.elemAt index set
      eqInterval = iterateInterval equi eqs
    in if deg >= 0 then eqInterval %+% interval else invert eqInterval

data Tuning a = Tuning { system::TuningSystem a, referenceFreq:: Frequency, referenceDegree:: (Degree, Int) }

-- TODO make instance of
instance (Interval a) => DegreeIndexable (Tuning a) Frequency where
freqOfDegree :: (Interval a) => Tuning a -> (Degree, Int) -> Frequency
freqOfDegree (Tuning ts@(TuningSystem intSet eqInt) refFreq (refDeg, refEq)) (deg, eq) =
  let diffEqs = refEq - eq
      eqInterval = iterateInterval eqInt diffEqs
      degInterval = (invert  $ ts !!! refDeg) `mappend` (ts !!! deg)
    in refFreq %>% (mappend eqInterval degInterval)
