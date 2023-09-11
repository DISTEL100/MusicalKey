module MusicalKey.Tuning.Mode ( module MusicalKey.Tuning.Mode) where
import MusicalKey.Pitch

data Mode a = Mode {root:: a, scale::[Int]} 

instance (Show a) => Show (Mode a) where
  show (Mode r s) = "Mode {root=" ++ show r ++", scale=" ++ show s ++ "}"

