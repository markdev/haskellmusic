module Main where
import Functions
import Helpers
import Euterpea

main =
  -- play $ times 3 instrumentSample
  play bottomRow

instrumentSample =
  topSample :=: bottomSample

topSample =
  instrument Sitar $
  c 4 qn :+: c 4 qn :+: d 4 qn :+: c 4 qn

bottomSample =
  instrument Woodblock $
  g 3 en :+: g 3 en :+: g 3 en :+: g 3 en :+:
  g 3 en :+: g 3 en :+: a 3 en


bottomRow =
  instrument Woodblock $
  avgNote (c, 4, en) 8 [0,1,2,3,4,5,6,7]

-- avgNote :: (b -> GHC.Real.Ratio Integer -> Music a, b, GHC.Real.Ratio Integer) -> Dur -> [Dur] -> Dur -> Music a
avgNote (n, o, nl) al [x,y] = n o nl :+: getRest y al nl :+: n o nl
avgNote (n, o, nl) al (x:y:ys) = n o nl :+: getRest x y nl :+: avgNote (n, o, nl) al (y:ys)

-- getRest :: Dur -> Dur -> Dur -> Music a
getRest current next dur =
  rest $ ((next - current) / 4) - dur
