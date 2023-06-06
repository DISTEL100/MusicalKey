module MusicalKey.TuningSystem.Tuning where

import Data.Group
import Data.Map (Map)
import Data.Map qualified as M
import MusicalKey.Interval
import MusicalKey.Pitch
import MusicalKey.TuningSystem

newtype MidiNote = MidiNote Int deriving (Enum, Show, Eq, Ord)
midiNote :: Int -> MidiNote
midiNote a
  | a > 127 = error "a MidiNote must be smaller then 128"
  | a < 0 = error "a MidiNote must be greater or equal 0"
  | otherwise = MidiNote a

data Tuning a b c
  = (IsPitch b, TuningSystem a b) => TunedByRef a (Pitch b) c
  | (IsPitch b, TuningSystem a b) => TunedByFunc a (Interval -> c)
  | (IsPitch b, TuningSystem a b) => TunedByMap (Map (Pitch b) c)
  | TunedDirect (Pitch b -> c)

type MidiTuning a b = Tuning a b MidiNote
type FreqTuning a b = Tuning a b Frequency

(!>!) :: FreqTuning a b -> Pitch b -> Frequency
TunedDirect tFunc !>! pitch = tFunc pitch
TunedByMap mapping !>! pitch = mapping M.! pitch
TunedByFunc tSystem tFunc !>! pitch = tFunc $ tSystem !%! pitch
TunedByRef tSystem refPitch refC !>! pitch =
  let interval = tSystem !%! pitch :: Interval
      refInterval = tSystem !%! refPitch :: Interval
   in refC <<> refInterval <>> interval

degreesInOctave ::(IsPitch b, TuningSystem a b) => a -> Int
degreesInOctave ts = degreesInOctave' ts (mempty::Pitch b) 0 
  where degreesInOctave' :: (IsPitch b, TuningSystem a b) => a -> Pitch b -> Int -> Int
        degreesInOctave' ts accP acc = 0

(!=!) :: MidiTuning a b -> Pitch b -> MidiNote
TunedDirect tFunc !=! pitch = tFunc pitch
TunedByMap mapping !=! pitch = mapping M.! pitch
TunedByFunc tSystem tFunc !=! pitch = tFunc $ tSystem !%! pitch
TunedByRef tSystem refPitch@(Pitch refP) refMidiNote@(MidiNote m) !=! pitch@(Pitch p)
  | refPitch == pitch = refMidiNote
  | rep refP == rep p = MidiNote $ m + (deg refP - deg p)
