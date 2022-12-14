module S14 where

import Lib
import qualified Data.Vector as V
import qualified Data.Set as S
import qualified Text.Parsec as P
import Data.Maybe

type Cave = S.Set Point2D

main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input
  let sandEntry = (500, 0) -- sand spawns here

  let pairs = concatMap (\x -> zip x (tail x)) pInput
  let lcave = concatMap (uncurry line2D) pairs
  let ((minx, _), maxb) = boundaries2D lcave
  let bounds = ((minx, 0), maxb) -- correct y min boundary to 0

  let cave = S.fromList lcave


  let (ncave, s) = dropSand True cave bounds sandEntry 0
  let (ncave2, s2) = dropSand False cave bounds sandEntry 0

  print2D $ S.toList ncave

  print s -- part 1
  print s2 -- part 2

dropSand :: Bool -> Cave -> (Point2D, Point2D) -> Point2D -> Int -> (Cave, Int)
dropSand bounded cave bounds@(_, (_, maxy)) start n = case simSand start of
    Nothing -> (cave, n)
    Just p -> dropSand bounded (S.insert p cave) bounds start (n + 1)
  where
    simSand :: Point2D -> Maybe Point2D
    simSand cur@(_, y)
      | S.member cur cave = Nothing -- blocked origin
      | not bounded && y > maxy = Just cur -- floor hit (if set)
      | bounded && not (checkBounds bounds down) = Nothing -- fall down (out of map)
      | not (S.member down cave) = simSand down -- fall down
      | bounded && not (checkBounds bounds ldown) = Nothing -- fall diag-left (out of map)
      | not (S.member ldown cave) = simSand ldown -- fall diag-left
      | bounded && not (checkBounds bounds rdown) = Nothing -- fall diag-right (out of map)
      | not (S.member rdown cave) = simSand rdown -- fall diag-right
      | otherwise = Just cur
      where
        down = step2D DirDown cur
        ldown = step2D DirLeft down
        rdown = step2D DirRight down

parseLine :: ParserT [Point2D]
parseLine = P.sepBy parsePoint (P.string " -> ")
  where
    parseInt = read <$> P.many P.digit
    parsePoint = do
      a <- parseInt
      P.char ','
      b <- parseInt
      return (a, b)
