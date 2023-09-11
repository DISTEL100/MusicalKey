module MusicalKey.Tuning.Mode ( module MusicalKey.Tuning.Mode) where
import MusicalKey.Pitch

data Mode  = Mode {root:: Int, scale::[Int]} deriving Show

scalePitch :: (IsPitch p) => Mode -> Pitch p -> Pitch p
scalePitch (Mode{root=r,scale=s}) p
  = let rooted = r + deg p :: Int
        size = length s
        dm = divMod (deg p) size
        newRep = rep p + fst dm
        newDeg = rooted + deg p + s!!snd dm
    in  toPitch newDeg newRep

