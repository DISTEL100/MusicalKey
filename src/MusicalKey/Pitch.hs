module MusicalKey.Pitch (module MusicalKey.Pitch) where 

import Data.Group (Group (invert))

type Equave = Int

type Degree = Int

class IsPitch a where
  degree :: Pitch a -> Degree
  equave :: Pitch a -> Equave
  equave _ = 0
  toPitch :: Degree -> Equave -> Pitch a

newtype Pitch a = Pitch a deriving (Show)

getIsPitch :: Pitch a -> a
getIsPitch (Pitch a) = a

convertPitch :: (IsPitch a, IsPitch b) => Pitch a -> Pitch b
convertPitch p = toPitch (degree p) (equave p)

pitchInt :: Int -> Int -> Pitch (Int, Int)
pitchInt a b = Pitch (a,b)

instance IsPitch (Int, Int) where
  degree = fst . getIsPitch
  equave = snd . getIsPitch
  toPitch d r = Pitch (d, r)

instance IsPitch Int where
  degree = getIsPitch
  toPitch i _ = Pitch i

instance IsPitch a => Eq (Pitch a) where
  p1 == p2 = equave p1 == equave p2 && degree p1 == degree p2

instance IsPitch a => Ord (Pitch a) where
   p1 <=  p2
    | equave p1 /= equave p2 = equave p1 <= equave p2
    | otherwise = degree p1 <= degree p2

instance IsPitch a => Semigroup (Pitch a) where
   p1 <>  p2 = toPitch (degree p1 + degree p2) (equave p1 + equave p2)

instance IsPitch a => Monoid (Pitch a) where
  mempty =  toPitch 0 0

instance IsPitch a => Group (Pitch a) where
  invert p = toPitch (-(degree p)) (-(equave p))
