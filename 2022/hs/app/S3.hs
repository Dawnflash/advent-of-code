module S3 where

import Data.Char
import Data.List

main :: String -> IO ()
main input = do
  let inputLines = lines input
  print $ sum $ priority . findCommon . splitInHalf <$> inputLines -- stage 1
  print $ sum $ priority . findCommon <$> makeTriplets [] inputLines -- stage 2
  where
    splitInHalf :: String -> [String]
    splitInHalf line = let (a, b) = splitAt (length line `div` 2) line in [a, b]

    priority :: Char -> Int
    priority x
      | o >= ord 'a' = o - ord 'a' + 1
      | otherwise = o - ord 'A' + 27
      where
        o = ord x

    findCommon :: [String] -> Char
    findCommon = head . foldl1 intersect

    -- stage 2
    makeTriplets :: [[String]] -> [String] -> [[String]]
    makeTriplets acc (a : b : c : t) = [a, b, c] : makeTriplets acc t
    makeTriplets acc _ = acc
