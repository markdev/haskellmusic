-- module HSoM.Examples.Interlude where
import Euterpea
import Data.List

main =
  play twinkle1


-- riff =
--   c 4 en :+: c 4 en :+: d 4 en :+: d 4 en

pcToQN :: PitchClass -> Music Pitch
pcToQN pc = note qn (pc, 4)

twinkle1 =
  line (map pcToQN [C,C,G,G,A,A]) :+: g 4 hn
