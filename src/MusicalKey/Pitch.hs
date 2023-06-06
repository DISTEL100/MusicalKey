module MusicalKey.Pitch where

import Data.Group (Group (invert))

class IsPitch a where
  deg :: a -> Int
  rep :: a -> Int
  rep _ = 0
  toPitch :: Int -> Int -> a

newtype Pitch a = Pitch a

instance IsPitch a => Eq (Pitch a) where
  Pitch p1 == Pitch p2 = rep p1 == rep p2 && deg p1 == deg p2

instance IsPitch a => Ord (Pitch a) where
  Pitch p1 <= Pitch p2
    | rep p1 /= rep p2 = rep p1 <= rep p2
    | otherwise = deg p1 <= deg p2

instance IsPitch a => Semigroup (Pitch a) where
  Pitch p1 <> Pitch p2 = Pitch $ toPitch (deg p1 + deg p2) (rep p1 + rep p2)

instance IsPitch a => Monoid (Pitch a) where
  mempty = Pitch $ toPitch 0 0

instance IsPitch a => Group (Pitch a) where
  invert (Pitch p) = Pitch $ toPitch (-(deg p)) (-(rep p))

instance IsPitch (Int, Int) where
  deg = fst
  rep = snd
  toPitch d r = (d, r)

iPitch :: Int -> Int -> Pitch (Int, Int)
iPitch d r = Pitch (d, r)

instance IsPitch Int where
  deg = id
  toPitch i _ = i
