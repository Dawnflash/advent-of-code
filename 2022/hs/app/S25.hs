module S25 where

import Lib (ParserT, parseLines)
import qualified Text.Parsec as P

main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input

  print $ toSNAFU $ sum $ map fromSNAFU pInput -- part 1

toSNAFU :: Int -> String
toSNAFU i = map conv $ reverse $ carry 0 $ reverse $ toPental i initP
  where
    conv :: Int -> Char
    conv (-2) = '='
    conv (-1) = '-'
    conv d = head $ show d
    initP = fromIntegral $ intLog 5 $ fromIntegral i
    toPental :: Int -> Int -> [Int]
    toPental i p
      | p < 0 = []
      | otherwise = d : toPental m (pred p)
      where
        (d, m) = i `divMod` (5 ^ p)
    carry :: Int -> [Int] -> [Int]
    carry 0 [] = []
    carry 1 [] = [1]
    carry i (h : t)
      | hi < 3 = hi : carry 0 t
      | otherwise = (hi - 5) : carry 1 t
      where
        hi = h + i

fromSNAFU :: String -> Int
fromSNAFU s = sum $ zipWith (\i c -> 5 ^ i * conv c) [0 ..] (reverse s)
  where
    conv '=' = -2
    conv '-' = -1
    conv c = read [c]

intLog :: Integer -> Integer -> Integer
intLog b i = intLog' b
  where
    intLog' x
      | x > i = 0
      | otherwise = succ $ intLog' (x * b)

parseLine :: ParserT String
parseLine = P.many P.anyChar
