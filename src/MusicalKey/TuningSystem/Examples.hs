module MusicalKey.TuningSystem.Examples (module MusicalKey.TuningSystem.Examples) where

import Data.Set qualified as Set
import MusicalKey.Interval
import MusicalKey.TuningSystem
import MusicalKey.TuningSystem.Tuning
import MusicalKey.TuningSystem.MidiTuning
import MusicalKey.Pitch (Pitch)

et12 :: Set.Set Interval
et12 = Set.fromList $ map Cent [100, 200 .. 1200]

createA440Tuning :: (TuningSystem a (Pitch b)) => a -> Tuning a (Pitch b)
createA440Tuning a = TunedByRef a (pitch 9 4) (Freq 440)

et12A440 = createA440Tuning et12

et12A440M64 :: MidiTuningByRef (TunedByRef (Set.Set Interval) Pitch) Pitch
et12A440M64 = MidiTuningByRef et12A440 (pitch 9 4) (MidiNote 64)
