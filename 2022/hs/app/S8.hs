module S8 where

import qualified Data.Vector as V
import Data.Char ( ord )
import Data.Maybe ( fromJust, isNothing )
import Lib

type Forest = V.Vector Int

main :: String -> IO ()
main input = do
  let inputLines = lines input
  let fsize@(width, height) = (length $ head inputLines, length inputLines)
  let forest = parseInput width V.empty input
  -- print $ isVisible forest fsize <$> [0..(length forest - 1)]
  print $ sum $ fromEnum . or . directionalAnalysis forest fsize isVisibleDir . point2DFromInt width <$> [0..(length forest - 1)] -- part 1
  print $ maximum $ fromEnum . product . directionalAnalysis forest fsize viewDistanceDir . point2DFromInt width <$> [0..(length forest - 1)] -- part 2

parseInput :: Int -> Forest -> String -> Forest
parseInput _ f [] = f
parseInput w f ('\n':is) = parseInput w f is
parseInput w f (i:is) = V.cons (ord i - ord '0') $ parseInput w f is

directionalAnalysis :: Forest -> Point2D -> (Direction2D -> Forest -> Point2D -> Int -> Point2D -> a) -> Point2D -> [a]
directionalAnalysis f dim@(w, _) fn p = [
  fn DirUp f dim refHeight p,
  fn DirDown f dim refHeight p,
  fn DirLeft f dim refHeight p,
  fn DirRight f dim refHeight p]
  where
    refHeight = f V.! point2DToInt w p

isVisibleDir :: Direction2D -> Forest -> Point2D -> Int -> Point2D -> Bool
isVisibleDir dir f dim@(w, _) refHeight p = let np = step2D dir p in
  not (checkBounds0 dim np) || (refHeight > f V.! point2DToInt w np && isVisibleDir dir f dim refHeight np)

viewDistanceDir :: Direction2D -> Forest -> Point2D -> Int -> Point2D -> Int
viewDistanceDir dir f dim@(w, _) refHeight p
  | not (checkBounds0 dim np) = 0
  | nh >= refHeight = 1
  | otherwise = 1 + viewDistanceDir dir f dim refHeight np
  where
    np = step2D dir p
    nh = f V.! point2DToInt w np
