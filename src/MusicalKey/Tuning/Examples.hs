{-# OPTIONS_GHC -Wno-type-defaults #-}

module MusicalKey.Tuning.Examples (module MusicalKey.Tuning.Examples) where

import Data.Ratio ((%))
import Data.Set qualified as Set
import MusicalKey.Interval (Frequency (..), Interval)
import MusicalKey.Pitch (Pitch (Pitch), pitchInt )
import MusicalKey.Tuning.Tuning
import MusicalKey.TuningSystem (tuningFromCents, tuningFromRatios)
import MusicalKey.Tuning.Mode

et12 :: Set.Set Interval
et12 = tuningFromCents [100, 200 .. 1200]

a4 :: Pitch (Int, Int)
a4 = Pitch (9, 4)

et12A440 :: TunedByRef Frequency
et12A440 = TunedByRef et12 a4 (Freq 440)

et12A440p1 :: Frequency
et12A440p1 = et12A440 !>! pitchInt 1 1

et12M64 :: TunedByRef MidiNote
et12M64 = TunedByRef et12 a4 (midiNote 69)

quint :: Set.Set Interval
quint = tuningFromRatios [3 % 2, 3 ^ 2 % 2 ^ 3, 3 ^ 3 % 2 ^ 4, 3 ^ 4 % 2 ^ 6, 3 ^ 5 % 2 ^ 7]

quintA440 :: TunedByRef Frequency
quintA440 = TunedByRef quint a4 (Freq 440)

chromaticEt12A440 :: Mode Frequency
chromaticEt12A440 = Mode (Root 0) (Scale [0..11]) et12A440

chromaticEt12M64 :: Mode MidiNote
chromaticEt12M64 = Mode (Root 0) (Scale [0..11]) et12M64
