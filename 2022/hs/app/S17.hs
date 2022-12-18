module S17 where

import Lib
import qualified Text.Parsec as P
import qualified Data.Set as S

-- obstacles, height, shifts
type State = (S.Set Point2D, Int, [Char], [Shape])
type Shape = [Point2D]


main :: String -> IO ()
main input = do
  let pInput = cycle $ head $ lines input
  -- - | + | J | I | o
  let shapes = cycle [[(0, 0), (1, 0), (2, 0), (3, 0)], [(1, 0), (0, -1), (1, -1), (2, -1), (1, -2)], [(0, 0), (1, 0), (2, 0), (2, -1), (2, -2)], [(0, 0), (0, -1), (0, -2), (0, -3)], [(0, 0), (1, 0), (0, -1), (1, -1)]]

  print $ take 10 shapes
  print $ take 100 pInput

  let seed = (S.empty, 0, pInput, shapes)

  let (b1,h1,_,_) = runState 100 seed 2022
  print $ S.size b1
  print h1

  let (b2,h2,_,_) = runState 100 seed 1000000000000
  print $ S.size b2
  print h2

(!!!) :: [a] -> Int -> a
[]     !!! _ = error "!!!: out of range"
(x:_ ) !!! 0 = x
(x:xs) !!! n = seq x (xs !!! pred n)

runState :: Int -> State -> Int -> State
runState n seed i
  | i < n = iterate placeShape seed !!! i
  | otherwise = runState n (trim b,h,m,s) $ i - n
  where
    (b,h,m,s) = iterate placeShape seed !!! n
    trim = S.filter (\(x, y) -> h + y < n)

-- return a moved shape if the move is legal, otherwise the original
tryMove :: S.Set Point2D -> (Point2D -> Point2D) -> Shape -> (Shape, Bool)
tryMove blockers moveF shape
  | all check nshape = (nshape, True) -- passed
  | otherwise = (shape, False)
  where
    nshape = map moveF shape
    -- boundary check + board collision check
    check p@(x, y) = x >= 0 && x < 7 && y <= 0 && S.notMember p blockers

placeShape :: State -> State
placeShape (blockers, height, shifts, shape:shapes) = placeShape' sInit shifts
  where
    sInit = map (\(x, y) -> (x + 2, y - height - 3)) shape -- initial shape placement
    -- horizontal shift -> vertical drop
    placeShape' :: Shape -> [Char] -> State
    placeShape' shape (shift:shifts)
      | moved = placeShape' nshape shifts
      | otherwise = (S.union blockers (S.fromList nshape), max (-nheight) height, shifts, shapes) -- landed
      where
        (nshape, moved) = moveShape shift shape
        nheight = pred . minimum $ snd <$> shape
    moveShape :: Char -> Shape -> (Shape, Bool)
    moveShape shift = dropShape . fst . blowShape shift
      where
        blowShape '>' = tryMove blockers (\(x, y) -> (x + 1, y))
        blowShape '<' = tryMove blockers (\(x, y) -> (x - 1, y))
        dropShape = tryMove blockers (\(x, y) -> (x, y + 1))
