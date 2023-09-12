module MusicalKey.Pitch (module MusicalKey.Pitch) where 

import Data.Group (Group (invert))

class IsPitch a where
  deg :: Pitch a -> Int
  rep :: Pitch a -> Int
  rep _ = 0
  toPitch :: Int -> Int -> Pitch a

newtype Pitch a = Pitch a deriving (Show)

getIsPitch :: Pitch a -> a
getIsPitch (Pitch a) = a

convertPitch :: (IsPitch a, IsPitch b) => Pitch a -> Pitch b
convertPitch p = toPitch (deg p) (rep p)

instance IsPitch (Int, Int) where
  deg = fst . getIsPitch
  rep = snd . getIsPitch
  toPitch d r = Pitch (d, r)

instance IsPitch Int where
  deg = getIsPitch
  toPitch i _ = Pitch i

instance IsPitch a => Eq (Pitch a) where
  p1 == p2 = rep p1 == rep p2 && deg p1 == deg p2

instance IsPitch a => Ord (Pitch a) where
   p1 <=  p2
    | rep p1 /= rep p2 = rep p1 <= rep p2
    | otherwise = deg p1 <= deg p2

instance IsPitch a => Semigroup (Pitch a) where
   p1 <>  p2 = toPitch (deg p1 + deg p2) (rep p1 + rep p2)

instance IsPitch a => Monoid (Pitch a) where
  mempty =  toPitch 0 0

instance IsPitch a => Group (Pitch a) where
  invert p = toPitch (-(deg p)) (-(rep p))

