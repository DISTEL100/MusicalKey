module MusicalKey.Tuning.Mode (module MusicalKey.Tuning.Mode) where

import MusicalKey.Pitch

newtype Root = Root Degree deriving Show
newtype Scale = Scale [Degree] deriving Show


scalePitch :: (IsPitch p) => Degree -> [Degree] -> Pitch p -> Pitch p
scalePitch r s p =
  let rooted = r + degree p :: Int
      size = length s
      dm = divMod rooted size
      newRep = equave p + fst dm
      newDeg = s !! snd dm
   in toPitch newDeg newRep


