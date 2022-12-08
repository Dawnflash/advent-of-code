module Lib where

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p s = case dropWhile p s of
  [] -> []
  s' -> w : splitWhen p s''
    where
      (w, s'') = break p s'

split :: Eq a => a -> [a] -> [[a]]
split d = splitWhen (== d)

-- 2D helpers

type Point2D = (Int, Int)
data Direction2D = DirUp | DirDown | DirLeft | DirRight

point2DFromInt :: Int -> Int -> Point2D
point2DFromInt w p = let (y, x) = divMod p w in (x, y)

point2DToInt :: Int -> Point2D -> Int
point2DToInt w (x, y) = w * y + x

neigh2D :: Direction2D -> Point2D -> Point2D -> Maybe Point2D
neigh2D d (w, h) (x, y) = neigh2D' d
  where
    neigh2D' DirUp
      | y <= 0 = Nothing
      | otherwise = Just (x, y - 1)
    neigh2D' DirDown
      | y + 1 >= h = Nothing
      | otherwise = Just (x, y + 1)
    neigh2D' DirLeft
      | x <= 0 = Nothing
      | otherwise = Just (x - 1, y)
    neigh2D' DirRight
      | x + 1 >= w = Nothing
      | otherwise = Just (x + 1, y)
