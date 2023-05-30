module MusicalKey.Tuning.Examples (module MusicalKey.Tuning.Examples) where

import Data.Set qualified as Set
import MusicalKey.Interval
import MusicalKey.Tuning

et12 :: Set.Set Interval
et12 = Set.fromList $ map Cent [100, 200 .. 1200]

octave :: Interval
octave = Cent 1200

createA440Tuning :: (TuningSystem a Pitch) => a -> TunedByReference a Pitch
createA440Tuning a = ByRef a (9 :: Degree, 4 :: Repeat) (Freq 440)

et12A440 :: TunedByReference (Set.Set Interval) Pitch
et12A440 = createA440Tuning et12
