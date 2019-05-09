module Main where
import Functions
import Helpers
import Euterpea
import Data.Char (ord, chr)

main = do
  -- play $ times 3 instrumentSample
  let music = topRow :=: middleRow :=: bottomRow
  let musicString = parrallelToString music
  putStrLn musicString
  play $ music

parrallelToString (m1 :=: m2) = generateRow m1 ++ "\n" ++ parrallelToString m2
parrallelToString m = generateRow m

generateRow (n1 :+: n2) = showPrimitive n1 ++ generateRow n2
generateRow (Modify (Instrument i) m) = show i ++ "\n" ++ generateRow m
generateRow n = showPrimitive n

showPrimitive (Prim (Rest x)) = replicate (ceiling (x * 64)) $ '_'
showPrimitive (Prim (Note x y)) = replicate (ceiling (x * 64)) $ chr 9608
showPrimitive _ = "_"



topRow =
  instrument Tuba $
  avgNote (g, 3, en) 8 [0,1,2,3,4,5,6,7]

middleRow =
  instrument Sitar $
  avgNote (g, 3, sfn) 8 [0,1,2,3,4,5,6,7,7.5,7.75]

bottomRow =
  instrument Woodblock $
  avgNote (c, 4, en) 8 [0,2,4,6,7]



avgNote (n, o, nl) al [x] =
  n o nl :+: getRest x al nl
avgNote (n, o, nl) al (x:y:ys) =
  n o nl :+: getRest x y nl :+: avgNote (n, o, nl) al (y:ys)

getRest current next dur =
  rest $ ((next - current) / 4) - dur
