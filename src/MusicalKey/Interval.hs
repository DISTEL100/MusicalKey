{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MusicalKey.Interval (module MusicalKey.Interval) where

import Data.Group (Abelian, Group (invert))
import GHC.Real ( Ratio((:%)), (%) )

newtype Frequency = Freq Float deriving (Enum, Ord, Eq, Show)

data Interval' = Cent Float | Ratio Rational

instance Semigroup Interval' where
  (Cent a) <> Cent b = Cent $ a + b
  (Ratio a) <> Ratio b = Ratio $ a * b
  _ <> _ = error "cant combine ratios with cents without rounding"

instance Monoid Interval' where
  mempty = Ratio (1 % 1)

instance Group Interval' where
  invert (Cent a) = Cent $ -a
  invert (Ratio (a :% b)) = Ratio $ b :% a

instance Abelian Interval'
