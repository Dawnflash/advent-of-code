module S4 where

import Lib

main :: String -> IO ()
main input = do
  let inputLines = lines input
  let ranges = map (map read . split '-') . split ',' <$> inputLines
  print $ sum $ isContained <$> ranges -- part 1
  print $ sum $ isOverlapped <$> ranges -- part 2

isContained :: [[Int]] -> Int
isContained [[al, ah], [bl, bh]]
  | al <= bl && ah >= bh || al >= bl && ah <= bh = 1
  | otherwise = 0

-- part 2
isOverlapped :: [[Int]] -> Int
isOverlapped [[al, ah], [bl, bh]]
  | al > bh || ah < bl = 0
  | otherwise = 1
