module Lib where

import Data.Either (rights, fromRight)
import qualified Text.Parsec as P
import Data.Maybe ( mapMaybe )

type ParserT a = P.Parsec String () a

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
data Direction2D = DirUp | DirDown | DirLeft | DirRight deriving (Eq, Show)

point2DFromInt :: Int -> Int -> Point2D
point2DFromInt w p = let (y, x) = divMod p w in (x, y)

point2DToInt :: Int -> Point2D -> Int
point2DToInt w (x, y) = w * y + x

step2DBounded :: Direction2D -> Point2D -> Point2D -> Maybe Point2D
step2DBounded d (w, h) p@(x, y)
  |    d == DirUp && y <= 0
    || d == DirDown && y + 1 >= h
    || d == DirLeft && x <= 0
    || d == DirRight && x + 1 >= w = Nothing
  | otherwise = Just $ step2D d p

step2D :: Direction2D -> Point2D -> Point2D
step2D d (x, y) = step2D' d
  where
    step2D' DirUp = (x, y - 1)
    step2D' DirDown = (x, y + 1)
    step2D' DirLeft = (x - 1, y)
    step2D' DirRight = (x + 1, y)

neighbors2DBounded :: Point2D -> Point2D -> [Point2D]
neighbors2DBounded dims p = mapMaybe (\d -> step2DBounded d dims p) [DirUp, DirDown, DirLeft, DirRight]

isNeighbor2D :: Point2D -> Point2D -> Bool
isNeighbor2D (x1, y1) (x2, y2) = abs (x1 - x2) < 2 && abs (y1 - y2) < 2

map2D :: (Int -> Int -> Int) -> Point2D -> Point2D -> Point2D
map2D f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

print2D :: [Point2D] -> IO ()
print2D v = mapM_ (printL [minx..maxx]) [miny..maxy]
  where
    minx = minimum $ fst <$> v
    maxx = maximum $ fst <$> v
    miny = minimum $ snd <$> v
    maxy = maximum $ snd <$> v
    printL :: [Int] -> Int -> IO ()
    printL [] y = putChar '\n'
    printL (x:xs) y = putChar (printC (x, y)) >> printL xs y
    printC :: Point2D -> Char
    printC x
      | x `elem` v = '█'
      | otherwise = '░'

-- parsing

parseLines :: P.Parsec String () a -> [String] -> [a]
parseLines p = rights . map (P.parse p "")
