module MusicalKey.Tuning.Mode (module MusicalKey.Tuning.Mode) where

import MusicalKey.Pitch
import MusicalKey.Tuning.Tuning (Tuning, (!>!))

newtype Root = Root Degree deriving Show
newtype Scale = Scale [Degree] deriving Show

data Mode out = forall t. (Tuning t out) => Mode {root::Root, scale:: Scale, tun :: t out }

scalePitch :: (IsPitch p) => Degree -> [Degree] -> Pitch p -> Pitch p
scalePitch r s p =
  let rooted = r + degree p :: Int
      size = length s
      dm = divMod rooted size
      newRep = equave p + fst dm
      newDeg = s !! snd dm
   in toPitch newDeg newRep

instance Tuning Mode out where
  Mode (Root r)  (Scale s) t !>! p = t !>! scalePitch r s p


