{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MusicalKey.Interval where

import Data.Group
import Data.Ratio (approxRational, (%))
import GHC.Float (fromRat)

newtype Frequency = Freq Float deriving (Enum, Ord, Eq, Show)

class (Ord a, Group a) => Interval a where
  (%%) :: Frequency -> Frequency -> a
  (%>%) :: Frequency -> a -> Frequency
  (Freq f) %>% a = Freq f %<% invert a
  (%<%) :: Frequency -> a -> Frequency
  (Freq f) %<% a = Freq f %>% invert a

iterateInterval :: Interval a => a -> Int -> a
iterateInterval interval iterations =
  let iterated = iterate (mappend interval) mempty !! iterations
   in if iterations >= 0 then iterated else invert iterated

(%+%) :: (Interval a) => a -> a -> a
(%+%) a b = a `mappend` b

(%-%) :: (Interval a) => a -> a -> a
(%-%) a b = a `mappend` invert b

newtype Cent = Cent Float deriving (Enum, Ord, Eq, Show)
instance Semigroup Cent where
  (Cent a) <> (Cent b) = Cent (a + b)
instance Monoid Cent where
  mempty = Cent 0.0
instance Group Cent where
  invert (Cent a) = Cent (-a)
instance Interval Cent where
  Freq a %% Freq b = Cent (1200 * logBase 2 (b / a))
  (Freq f) %>% Cent a = Freq (f * 2 ** (a / 1200))

newtype FreqRatio = FreqRatio Rational deriving (Enum, Ord, Eq, Show)
instance Semigroup FreqRatio where
  FreqRatio a <> FreqRatio b = FreqRatio (a + b)
instance Monoid FreqRatio where
  mempty = FreqRatio (1 % 1)
instance Group FreqRatio where
  invert (FreqRatio a) = FreqRatio (recip a)
instance Interval FreqRatio where
  Freq a %% Freq b = FreqRatio (approxRational (a / b) 0.0000001)
  (Freq f) %>% FreqRatio a = Freq (f * fromRat a)
