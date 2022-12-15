module S15 where

import Lib
import qualified Text.Parsec as P
import qualified Data.Set as S


main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input -- [sensor, beacon]
  let sensors = map (\(s, b) -> (s, distance2DManhattan s b)) pInput -- sensors with range

  print $ S.size $ foldl (add 2000000) S.empty sensors S.\\ S.fromList (map snd pInput) -- part 1
  print $ p2tune $ search (0, 4000000) sensors -- part 2


-- add exclusion points to the set (sensor, beacon)
-- subtract sensor and beacon points
add :: Int -> S.Set Point2D -> (Point2D, Int) -> S.Set Point2D
add yTarget pts (s@(sx, sy), range)
  | spread < 0 = pts
  | otherwise = S.union pts $ S.fromList $ line2D (sx - spread, yTarget) (sx + spread, yTarget)
  where
    spread = range - abs (sy - yTarget)

-- part 2
p2tune :: Point2D -> Integer
p2tune (x, y) = 4000000 * fromIntegral x + fromIntegral y

search :: (Int, Int) -> [(Point2D, Int)] -> Point2D
search (lo, hi) sensors = searchP (lo, lo) sensors
  where
    searchP :: Point2D -> [(Point2D, Int)] -> Point2D
    searchP p [] = p -- all restrictions passed
    searchP p@(_, y) ((s@(sx, sy), range):ss)
      | ps > range = searchP p ss
      | nextX > hi = searchP (lo, y + 1) sensors -- new line
      | otherwise = searchP (nextX, y) sensors
      where
        ps = distance2DManhattan p s -- |p, s|
        spread = range - abs (sy - y) -- horizontal sensor range at the given line
        nextX = sx + spread + 1 -- next x out of sensor's range

parseLine :: ParserT (Point2D, Point2D)
parseLine = do
  P.string "Sensor at x="
  sx <- parseInt
  P.string ", y="
  sy <- parseInt
  P.string ": closest beacon is at x="
  bx <- parseInt
  P.string ", y="
  by <- parseInt
  return ((sx, sy), (bx, by))
