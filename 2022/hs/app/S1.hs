module S1 where

import Data.List

main :: String -> IO ()
main input = do
  let calories = reverse $ sort $ foldl wrap [] $ lines input
  let e1 : e2 : e3 : _ = calories
  print e1 -- stage 1
  print (e1 + e2 + e3) -- stage 2
  where
    wrap :: [Int] -> String -> [Int]
    wrap l "" = 0 : l
    wrap [] x = [read x]
    wrap (h : t) x = (read x + h) : t
