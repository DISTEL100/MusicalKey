module MusicalKey.Tuning.Mode (module MusicalKey.Tuning.Mode) where

import MusicalKey.Pitch
import MusicalKey.Tuning.Tuning (Tuning)

data Mode out = Mode {root :: Degree, scale :: [Degree], tuning :: forall t . (Tuning t out) => t } 

scalePitch :: (IsPitch p) => Mode out -> Pitch p -> Pitch p
scalePitch (Mode {root = r, scale = s}) p =
  let rooted = r + degree p :: Int
      size = length s
      dm = divMod (degree p) size
      newRep = equave p + fst dm
      newDeg = rooted + degree p + s !! snd dm
   in toPitch newDeg newRep
