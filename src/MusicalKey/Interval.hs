{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MusicalKey.Interval where

import           Data.Group
import           Data.Ratio (approxRational, (%))

newtype Frequency = Freq Float deriving (Enum, Ord, Eq, Show)

newtype Cent = Cent Float deriving (Enum, Ord, Eq, Show)

newtype FreqRatio = FreqRatio Rational deriving (Enum, Ord, Eq, Show)

class (Ord a, Group a) => Interval a where
  (%%) :: Frequency -> Frequency -> a

instance Semigroup Cent where
  (Cent a) <> (Cent b) = Cent (a + b)
instance Monoid Cent where
  mempty = Cent 0.0
instance Group Cent where
  invert (Cent a) = Cent (-a)
instance Interval Cent where
  Freq a %% Freq b = Cent (1200 * logBase 2 (b / a))

instance Semigroup FreqRatio where
  FreqRatio a <> FreqRatio b = FreqRatio (a + b)
instance Monoid FreqRatio where
  mempty = FreqRatio (1 % 1)
instance Group FreqRatio where
  invert (FreqRatio a) = FreqRatio (recip a)
instance Interval FreqRatio where
  Freq a %% Freq b = FreqRatio (approxRational (a / b) 0.0000001)
