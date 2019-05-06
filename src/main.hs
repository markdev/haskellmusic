module Main where
import Functions
import Helpers
import Euterpea

main =
  play $ times 3 instrumentSample

instrumentSample =
  topSample :=: bottomSample

topSample =
  instrument Sitar $
  c 4 qn :+: c 4 qn :+: d 4 qn :+: c 4 qn
  -- note qn ((C, 4) :: Pitch)
  -- play $ times 3 $ c 4 qn
  -- play $ c 4 qn

bottomSample =
  instrument Woodblock $
  g 3 en :+: g 3 en :+: g 3 en :+: g 3 en :+:
  g 3 en :+: g 3 en :+: a 3 en
