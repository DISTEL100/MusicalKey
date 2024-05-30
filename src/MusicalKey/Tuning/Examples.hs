{-# OPTIONS_GHC -Wno-type-defaults #-}

module MusicalKey.Tuning.Examples (module MusicalKey.Tuning.Examples) where

import Data.Ratio ((%))
import Data.Set qualified as Set
import MusicalKey.Interval (Interval)
import MusicalKey.TuningSystem (sortedCentSet, ratioSet)
import MusicalKey.Pitch

et12 :: Set.Set Interval
et12 = sortedCentSet [100, 200 .. 1200]

a4 :: WPitch
a4 = (9, 4)

quint :: Set.Set Interval
quint = ratioSet [3 % 2, 3 ^ 2 % 2 ^ 3, 3 ^ 3 % 2 ^ 4, 3 ^ 4 % 2 ^ 6, 3 ^ 5 % 2 ^ 7]
