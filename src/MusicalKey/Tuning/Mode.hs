module MusicalKey.Tuning.Mode (module MusicalKey.Tuning.Mode) where

import MusicalKey.Pitch
import MusicalKey.Tuning.Tuning (Tuning, (!>!))

data Mode out = forall t. (Tuning t out) => Mode Degree [Degree] t

root :: Mode out -> Degree
root (Mode r _ _) = r

scale :: Mode out -> [Degree]
scale (Mode _ s _) = s

scalePitch :: (IsPitch p) => Degree -> [Degree] -> Pitch p -> Pitch p
scalePitch r s p =
  let rooted = r + degree p :: Int
      size = length s
      dm = divMod (degree p) size
      newRep = equave p + fst dm
      newDeg = rooted + degree p + s !! snd dm
   in toPitch newDeg newRep

instance Tuning (Mode out) out where
  Mode r s t !>! p = t !>! scalePitch r s p


