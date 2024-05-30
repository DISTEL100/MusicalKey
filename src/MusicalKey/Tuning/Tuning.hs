module MusicalKey.Tuning.Tuning (module MusicalKey.Tuning.Tuning) where
import MusicalKey.TuningSystem
import MusicalKey.Interval
import MusicalKey.Pitch
import Data.Data
import Data.Group


newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)

midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

-- Tunings
class Tuning sys pitch out where
  (!%%) :: sys -> pitch -> out

tuneByRef :: (TuningSystem sys pitch) => Proxy sys -> (pitch, Frequency) -> pitch -> Frequency
tuneByRef sys (refPitch, refFreq) pitch =
  let refInterval = sys !% refPitch
      interval = sys !% pitch
      intervalBetween = interval <> invert refInterval
   in refFreq <>> intervalBetween

newtype TunedByRef sys pitch out = TunedByRef (pitch, out)

instance (TuningSystem sys pitch) => Tuning (TunedByRef sys pitch Frequency) pitch Frequency where
  (TunedByRef ref) !%% pitch = tuneByRef (Proxy :: Proxy sys) ref pitch

-- Example ET12 A440
et12A440 :: TunedByRef ET12 WPitch Frequency
et12A440 = TunedByRef ((9,4), Freq 440)
