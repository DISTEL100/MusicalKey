{-# OPTIONS_GHC -Wno-type-defaults #-}
module MusicalKey.Tuning.Examples (module MusicalKey.Tuning.Examples) where

import Data.Set qualified as Set
import MusicalKey.Interval ( Interval(Cent, Ratio), Frequency(Freq) )
import MusicalKey.Tuning.Tuning
    ( FreqTuning, MidiTuning, Tuning(TunedByRef), midiNote )
import MusicalKey.Pitch (Pitch (Pitch))
import Data.Ratio ((%))
import Data.List (sort)

et12 :: Set.Set Interval
et12 = Set.fromList $ map Cent [100, 200 .. 1200]

a4 :: Pitch (Int,Int)
a4 = Pitch (9,4)

et12A440 :: FreqTuning 
et12A440 = TunedByRef et12 a4 (Freq 440)

et12M64 :: MidiTuning 
et12M64 = TunedByRef et12 a4 (midiNote 69)

quint :: Set.Set Interval
quint = Set.fromList $ map Ratio (sort [3%2, 3^2%2^3, 3^3%2^4, 3^4%2^6, 3^5%2^7  ])

quintA440 :: FreqTuning
quintA440 =TunedByRef quint a4 (Freq 440)
