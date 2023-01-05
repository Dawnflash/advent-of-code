module S25 where

import qualified Data.Vector.Unboxed as V
import Lib (ParserT, parseLines)
import qualified Text.Parsec as P

main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input

  print $ toSNAFU $ sum $ map fromSNAFU pInput -- part 1

toSNAFU :: Int -> String
toSNAFU 0 = "0"
toSNAFU i = vprint $ toSNAFU' i initP (V.fromList $ replicate (initP + 2) 0)
  where
    initP = fromIntegral $ intLog 5 $ fromIntegral i
    vprint = V.foldr (\c acc -> conv c : acc) "" . V.dropWhile (== 0) . V.reverse
      where
        conv (-2) = '='
        conv (-1) = '-'
        conv d = head $ show d
    toSNAFU' :: Int -> Int -> V.Vector Int -> V.Vector Int
    toSNAFU' i p v
      | p < 0 = v
      | otherwise = toSNAFU' m (pred p) $ carry p d $ v V.// [(p, d)]
      where
        (d, m) = i `divMod` (5 ^ p)
        carry p d v
          | d < 3 = v
          | otherwise = carry (succ p) sParent $ v V.// [(p, d - 5), (succ p, sParent)]
          where
            sParent = succ $ v V.! succ p

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
