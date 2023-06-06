module MusicalKey.TuningSystem.Examples (module MusicalKey.TuningSystem.Examples) where

import Data.Set qualified as Set
import MusicalKey.Interval
import MusicalKey.TuningSystem.Tuning
import MusicalKey.Pitch (Pitch (Pitch))

et12 :: Set.Set Interval
et12 = Set.fromList $ map Cent [100, 200 .. 1200]
 
a4 :: Pitch (Int,Int)
a4 = Pitch (9,4)

et12A440 :: FreqTuning (Set.Set Interval) (Int,Int)
et12A440 = TunedByRef et12 a4 (Freq 440)

et12M64 :: MidiTuning (Set.Set Interval) (Int,Int)
et12M64 = TunedByRef et12 a4 (midiNote 69)
