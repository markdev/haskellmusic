module Main where
import Functions
import Helpers
import Euterpea

main = do
  -- play $ times 3 instrumentSample
  let music = topRow :=: bottomRow :=: middleRow
  let musicString = getDisplay music
  putStrLn musicString
  play $ music

getDisplay (m1 :=: m2) = generateRow m1 ++ "\n" ++ getDisplay m2
getDisplay m = generateRow m

generateRow modify control a = "instrument" ++ generateRow n
generateRow (n1 :+: n) = "X" ++ generateRow n2
generateRow n2 = "O"


topRow =
  instrument Tuba $
  avgNote (g, 3, en) 8 [0,1,2,3,4,5,6,7] den

middleRow =
  instrument Sitar $
  avgNote (g, 3, en) 8 [0,1,2,3,4,5,6,7] en

bottomRow =
  instrument Woodblock $
  avgNote (c, 4, en) 8 [0,1,2,3,4,5,6,7] 0.0

-- avgNote :: (b -> GHC.Real.Ratio Integer -> Music a, b, GHC.Real.Ratio Integer) -> Dur -> [Dur] -> Dur -> Music a
avgNote (n, o, nl) al list off = rest off :+: avgNote' (n, o, nl) al list
avgNote' (n, o, nl) al [x,y] = n o nl :+: getRest y al nl :+: n o nl
avgNote' (n, o, nl) al (x:y:ys) = n o nl :+: getRest x y nl :+: avgNote' (n, o, nl) al (y:ys)

-- getRest :: Dur -> Dur -> Dur -> Music a
getRest current next dur =
  rest $ ((next - current) / 4) - dur
