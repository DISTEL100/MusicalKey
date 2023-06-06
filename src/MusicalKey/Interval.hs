module MusicalKey.Interval (module MusicalKey.Interval) where

import Data.Group (Abelian, Group (invert), (~~))
import Data.Ratio
import GHC.Real (Ratio ((:%)))

newtype Frequency = Freq Float deriving (Enum, Ord, Eq, Show)

data Interval = Cent Float | Ratio Rational deriving (Show)

instance Eq Interval where
  (Cent a) == Cent b = a == b
  (Ratio a) == Ratio b = a == b
  a@(Ratio _) == b = a == approxRatio b
  a@(Cent _) == b = a == approxCent b

instance Ord Interval where
  (Cent a) <= Cent b = a <= b
  (Ratio a) <= Ratio b = a <= b
  a@(Ratio _) <= b = a <= approxRatio b
  a@(Cent _) <= b = a <= approxCent b

instance Semigroup Interval where
  (Cent a) <> Cent b = Cent $ a + b
  (Ratio a) <> Ratio b = Ratio $ a * b
  a@(Ratio _) <> b = a <> approxRatio b
  a@(Cent _) <> b = a <> approxCent b

instance Monoid Interval where
  mempty = Ratio (1 % 1)

instance Group Interval where
  invert (Cent a) = Cent $ -a
  invert (Ratio (a :% b)) = Ratio $ b :% a

instance Abelian Interval

cents :: Frequency -> Frequency -> Interval
cents (Freq f1) (Freq f2) = Cent $ 1200 * logBase 2 (f2 / f1)

ratio :: Frequency -> Frequency -> Interval
ratio (Freq f1) (Freq f2) = Ratio $ approxRational (f2 / f1) 0.000001

(<>>) :: Frequency -> Interval -> Frequency
Freq f <>> Cent b = Freq $ f * 2 ** (b / 1200)
Freq f <>> Ratio b = Freq $ f * fromRational b

(<<>) :: Frequency -> Interval -> Frequency
Freq f <<> Cent b = Freq $ f * 2 ** (-b / 1200)
Freq f <<> Ratio (a :% b) = Freq $ f * fromRational (b :% a)

approxRatio :: Interval -> Interval
approxRatio (Ratio a) = Ratio a
approxRatio (Cent a) = Ratio $ approxRational (2 ** (a / 1200)) 0.000001

approxCent :: Interval -> Interval
approxCent (Cent a) = Cent a
approxCent (Ratio (f1 :% f2)) = cents (Freq $ fromInteger f1) (Freq $ fromInteger f2)

memptyOf :: Interval -> Interval
memptyOf i = i ~~ i
