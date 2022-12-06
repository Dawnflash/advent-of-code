module S2 where

import Data.Char

main :: String -> IO ()
main input = do
  let inputLines = lines input
  print $ foldl addLineScore 0 inputLines -- stage 1
  print $ foldl addLineScore2 0 inputLines -- stage 2
  where
    addLineScore a = (+ a) . score . map head . words

    score (a : [b]) =
      let (na, nb) = normalize a b
       in scoreMove nb + scoreMatchup na nb

    normalize a b = (ord a - ord 'A', ord b - ord 'X')

    scoreMove = succ

    scoreMatchup a b = 3 * mod (b - a + 1) 3

    -- 2nd part
    addLineScore2 a = (+ a) . score2 . map head . words

    score2 (a : [b]) =
      let (na, nb) = normalize a b
          cnb = mod (na + nb - 1) 3
       in scoreMove cnb + scoreMatchup na cnb
