module MusicalKey.Tuning () where

import Data.Group (Group (pow), (~~))
import Data.Ratio
import Data.Set qualified as Set
import MusicalKey.Interval

data TuningSystem a = TuningSystem {intervalSet :: Set.Set a, equivalentInterval :: a}
  deriving (Show)

intervalByDegree :: Group a => TuningSystem a -> Int -> a
intervalByDegree (TuningSystem intervalSet eqInterval) degree =
  let size = Set.size intervalSet :: Int
      degreeNormalized = degree - 1
      interval = Set.elemAt (degreeNormalized `mod` size) intervalSet
      eqClass = pow eqInterval (degreeNormalized `div` size)
   in eqClass <> interval

intervalByPitch :: (Group x) => TuningSystem x -> (Int, Int) -> x
intervalByPitch ts@(TuningSystem _ equivalentInterval) (deg, oct) = intervalByDegree ts deg <> pow equivalentInterval oct

data Tuning a = Tuning
  { system :: TuningSystem a
  , referenceFreq :: MusicalKey.Interval.Frequency
  , referenceDegree :: (Int, Int)
  }
  deriving (Show)

freqByDegree :: (Interval a) => Tuning a -> Int -> Frequency
freqByDegree (Tuning ts@(TuningSystem _ eqInt) refFreq (refDeg, refEq)) deg =
  let degInterval = intervalByDegree ts deg ~~ intervalByDegree ts refDeg
   in refFreq %>% (degInterval ~~ pow eqInt refEq)

freqByPitch :: (Interval a) => Tuning a -> (Int, Int) -> Frequency
freqByPitch t@(Tuning ts@(TuningSystem _ eqInt) _ _) (deg, oct) =
  freqByDegree t deg %>% pow eqInt oct

edoTuning :: Int -> TuningSystem Cent
edoTuning stepsInOctave =
  let intervals = map (\x -> fromIntegral x / fromIntegral stepsInOctave * 1200) [1 .. stepsInOctave]
      cents = map Cent intervals
   in TuningSystem (Set.fromList cents) (Freq 1.0 %% Freq 2.0)

et12 :: TuningSystem Cent
et12 = edoTuning 12

et12a440 :: Tuning Cent
et12a440 = Tuning et12 (Freq 440) (9, 4)

fromCents :: [Float] -> TuningSystem Cent
fromCents cents = TuningSystem (Set.fromList $ map Cent cents) (Cent $ maximum cents)

fromFreqRatios :: [Rational] -> TuningSystem FreqRatio
fromFreqRatios ratios = TuningSystem (Set.fromList $ map FreqRatio ratios) (FreqRatio $ maximum ratios)

pyth :: TuningSystem FreqRatio
pyth = fromFreqRatios [9 % 8, 81 % 64, 4 % 3, 3 % 2, 27 % 16, 243 % 128, 2]

pythA440 :: Tuning FreqRatio
pythA440 = Tuning pyth (Freq 440) (9, 4)

