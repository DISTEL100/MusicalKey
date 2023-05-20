{-# LANGUAGE GADTs #-}

module MusicalKey.Scale (Tuning, Scale, Degree, (!!!), et12, pythagoreian) where

import           Data.Group
import           Data.Ratio
import qualified Data.Set            as Set
import           MusicalKey.Interval
kk
instance (Show a, Ord a) => Show (Tuning a) where
  show (Intervals set) = "Intervals " ++ show (Set.toAscList set)

fromCents :: [Float] -> Tuning Cent
fromCents cents = Intervals $ Set.fromList $ map Cent cents
fromFreqRatios :: [Rational] -> Tuning FreqRatio
fromFreqRatios ratios = Intervals $ Set.fromList $ map FreqRatio ratios

et12 :: Tuning Cent
et12 = fromCents [100,200..1200]
pythagoreian :: Tuning FreqRatio
pythagoreian = fromFreqRatios [9 % 8, 81 % 64, 4 % 3, 3 % 2, 27 % 16, 243 % 128, 2]

data Scale a where
  IntervalScale :: (Interval a) => Set.Set a -> Scale a
  TunedScale :: (Interval a) => Tuning a -> [Int] -> Scale a
  FreqScale :: [Frequency] -> Scale FreqRatio

type Degree = Int

octaveIndex :: (Interval a) => [a] -> Int -> a
octaveIndex list i = if i >= 0 then expandedList !! i
                     else map invert expandedList !! (-i)
                    where expandedList = mempty : concat (iterate (map (<> maximum list)) list)

(!!!) :: (Interval a) => Scale a -> Degree -> a
(IntervalScale set) !!! n                = octaveIndex (Set.toAscList set) n
(TunedScale (Intervals set) steps) !!! n = octaveIndex (Set.toAscList set) n

