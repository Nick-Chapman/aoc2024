module Day2 (main) where

import Misc (check)
import Par4 (parse,Par,separated,nl,int,ws1)

main :: IO ()
main = do
  sam <- parse gram <$> readFile "input/day2.sample"
  inp <- parse gram <$> readFile "input/day2.input"
  print ("day2, part1 (sample)", check 2 $ part1 sam)
  print ("day2, part1", check 257 $ part1 inp)
  print ("day2, part2 (sample)", check 4 $ part2 sam)
  print ("day2, part2", check 328 $ part2 inp)

  where
    part1 = count safe
    part2 = count dampenedSafe

    safe r = slowlyIncreasing r || slowlyDecreasing r
    dampenedSafe r = any safe (dampened r)

type Report = [Int]

gram :: Par [Report]
gram = separated nl (separated ws1 int)

count :: (Report -> Bool) -> [Report] -> Int
count pred rs = length (filter pred rs)

slowlyIncreasing :: Report -> Bool
slowlyIncreasing r = all id [ d>=1 && d<=3 | (a,b) <- zip r (tail r), let d = b-a ]

slowlyDecreasing :: Report -> Bool
slowlyDecreasing r = all id [ d>=1 && d<=3 | (a,b) <- zip r (tail r), let d = a-b ]

dampened :: Report -> [Report]
dampened = \case
  [] -> []
  x:xs -> xs : [ x:ys | ys <- dampened xs ]
