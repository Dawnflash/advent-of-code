module Lib where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
  [] -> []
  s' -> w : splitWhen p s''
    where
      (w, s'') = break p s'

split :: Eq a => a -> [a] -> [[a]]
split d = splitWhen (== d)
