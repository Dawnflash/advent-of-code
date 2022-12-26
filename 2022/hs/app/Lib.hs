module Lib where

import Data.Either (fromRight, rights)
import Data.Maybe (catMaybes, mapMaybe)
import qualified Text.Parsec as P

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

-- min x and y is (0, 0)
checkBounds0 :: Point2D -> Point2D -> Bool
checkBounds0 (w, h) = checkBounds ((0, 0), (w - 1, h - 1))

checkBounds :: (Point2D, Point2D) -> Point2D -> Bool
checkBounds ((xmin, ymin), (xmax, ymax)) (x, y) = x >= xmin && x <= xmax && y >= ymin && y <= ymax

step2D :: Direction2D -> Point2D -> Point2D
step2D d (x, y) = step2D' d
  where
    step2D' DirUp = (x, y - 1)
    step2D' DirDown = (x, y + 1)
    step2D' DirLeft = (x - 1, y)
    step2D' DirRight = (x + 1, y)

move2D :: [Direction2D] -> Point2D -> Point2D
move2D ds p = foldl (flip step2D) p ds

neighbors2D :: Point2D -> [Point2D]
neighbors2D p = map (`step2D` p) [DirUp, DirDown, DirLeft, DirRight]

isNeighbor2D :: Point2D -> Point2D -> Bool
isNeighbor2D (x1, y1) (x2, y2) = abs (x1 - x2) < 2 && abs (y1 - y2) < 2

map2D :: (Int -> Int -> Int) -> Point2D -> Point2D -> Point2D
map2D f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

-- (upper left, bottom right)
boundaries2D :: (Foldable f, Functor f) => f Point2D -> (Point2D, Point2D)
boundaries2D ps = ((minimum $ fst <$> ps, minimum $ snd <$> ps), (maximum $ fst <$> ps, maximum $ snd <$> ps))

-- return a list of points corresponding to a line between two points
line2D :: Point2D -> Point2D -> [Point2D]
line2D (x1, y1) (x2, y2)
  | x1 == x2 = [(x1, y) | y <- [min y1 y2 .. max y1 y2]]
  | y1 == y2 = [(x, y1) | x <- [min x1 x2 .. max x1 x2]]
  | otherwise = [] -- no support for slanted lines

distance2DManhattan :: Point2D -> Point2D -> Int
distance2DManhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

print2D :: [Point2D] -> IO ()
print2D v = mapM_ (printL [minx .. maxx]) [miny .. maxy]
  where
    ((minx, miny), (maxx, maxy)) = boundaries2D v
    printL :: [Int] -> Int -> IO ()
    printL [] y = putChar '\n'
    printL (x : xs) y = putChar (printC (x, y)) >> printL xs y
    printC :: Point2D -> Char
    printC x
      | x `elem` v = '█'
      | otherwise = '░'

-- print boolean vector (with width)
print2DL :: Int -> [Bool] -> IO ()
print2DL w xs = print2D $ catMaybes $ zipWith fn [0 ..] xs
  where
    fn _ False = Nothing
    fn i True = Just $ point2DFromInt w i

-- N-Dimensional points

-- x y z
type Point = [Int]

isAdjacentND :: Point -> Point -> Bool
isAdjacentND a b = (== 1) . sum $ zipWith (\a b -> abs (a - b)) a b

isBoundedND :: [(Int, Int)] -> Point -> Bool
isBoundedND bounds p = and $ zipWith (\(lo, hi) x -> x >= lo && x <= hi) bounds p

isBoundedExclND :: [(Int, Int)] -> Point -> Bool
isBoundedExclND bounds p = and $ zipWith (\(lo, hi) x -> x > lo && x < hi) bounds p

neighborsND :: Point -> [Point]
neighborsND p = [x | i <- [0 .. length p - 1], di <- [1, -1], let x = take i p ++ [(p !! i) + di] ++ drop (i + 1) p]

-- parsing

parseLines :: ParserT a -> [String] -> [a]
parseLines p = rights . map (P.parse p "")

parseInt :: ParserT Int
parseInt = read <$> (P.optional (P.char '-') >> P.many1 P.digit)
