module S17 where

import Lib
import qualified Text.Parsec as P
import qualified Data.Set as S
import qualified Data.Map as M
import Data.List (scanl', iterate')
import Data.Maybe (fromJust, listToMaybe)

-- obstacles, height, shifts
type State = (S.Set Point2D, Int, Int, Int, [Char], [Shape])
type Shape = [Point2D]

height :: State -> Int
height (_,h,_,_,_,_) = h


main :: String -> IO ()
main input = do
  let lInput = head $ lines input
  let pInput = cycle lInput
  -- - | + | J | I | o
  let _shapes = [[(0, 0), (1, 0), (2, 0), (3, 0)], [(1, 0), (0, -1), (1, -1), (2, -1), (1, -2)], [(0, 0), (1, 0), (2, 0), (2, -1), (2, -2)], [(0, 0), (0, -1), (0, -2), (0, -3)], [(0, 0), (1, 0), (0, -1), (1, -1)]]
  let shapes = cycle _shapes
  let nshapes = length _shapes
  let nmoves = length lInput

  let seed = (S.empty, 0, 0, 0, pInput, shapes)

  print $ eval nshapes nmoves seed 2022 -- part 1
  print $ eval nshapes nmoves seed 1000000000000 -- part 2

-- evaluate board after n moves
eval :: Int -> Int -> State -> Int -> Int
eval nshapes nmoves state n
  | Just ((i1, h1), (i2, h2)) <- loop = evalLoop i1 h1 (i2 - i1) (h2 - h1)
  | otherwise = height $ iterate placeShape state !! n
  where
    loop = findLoop nshapes nmoves state
    evalLoop start starth di dh = finh + dh * skips
      where
        (skips, rem) = (n - start) `divMod` di
        finh = height $ iterate placeShape state !! (start + rem)

-- find a loop in the game identified by 2x (index, height) with identical board states
findLoop :: Int -> Int -> State -> Maybe ((Int, Int), (Int, Int))
findLoop nshapes nmoves = findLoop'
  where
    findLoop' state@(_, oh, onh, onm, _, _) = listToMaybe loop
      where
        next s = iterate placeShape s !! nshapes
        states = iterate next state
        smap = scanl' (flip $ uncurry M.insert) M.empty $ [((nm `mod` nmoves, nh, board), (i, h)) | (i, (board, h, nh, nm, _, _)) <- zip [0..] states]
        loop = [((ri * nshapes, rh), (i * nshapes, h)) | (i, (h, Just (ri, rh))) <- zip [0..] $ zipWith search states smap]
        search (b, h, nh, nm, _, _) m = (h, M.lookup (nm `mod` nmoves, nh, b) m)

-- return a moved shape if the move is legal, otherwise the original
tryMove :: S.Set Point2D -> (Point2D -> Point2D) -> Shape -> (Shape, Bool)
tryMove blockers moveF shape
  | all check nshape = (nshape, True) -- passed
  | otherwise = (shape, False)
  where
    nshape = map moveF shape
    -- boundary check + board collision check
    check p@(x, y) = x >= 0 && x < 7 && y <= 0 && S.notMember p blockers

-- place one tetris block on the board
placeShape :: State -> State
placeShape (blockers, height, normheight, nmoves, shifts, shape:shapes) = placeShape' sInit shifts nmoves
  where
    sInit = map (\(x, y) -> (x + 2, y - normheight - 3)) shape -- initial shape placement
    -- horizontal shift -> vertical drop
    placeShape' :: Shape -> [Char] -> Int -> State
    placeShape' shape (shift:shifts) nmoves
      | moved = placeShape' nshape shifts $ succ nmoves
      | otherwise = (blockers', height', normheight' - offset, succ nmoves, shifts, shapes) -- landed
      where
        (nshape, moved) = moveShape shift shape
        sheight = pred . minimum $ snd <$> nshape
        normheight' = max (-sheight) normheight
        height' = height + (normheight' - normheight)
        (blockers', offset) = normalize $ S.union blockers (S.fromList nshape)
        normalize set = (S.map (\(x, y) -> (x, y - miny)) fset, -miny)
          where
            fset = S.filter (\(_, y) -> normheight' + y < 100) set -- 100 is fairly arbitrary here to mask my lack of proper D|BFS
            miny = snd . point2DFromInt 7 . fst . fromJust $ S.maxView $ S.map (point2DToInt 7) fset
    moveShape :: Char -> Shape -> (Shape, Bool)
    moveShape shift = dropShape . fst . blowShape shift
      where
        blowShape '>' = tryMove blockers (\(x, y) -> (x + 1, y))
        blowShape '<' = tryMove blockers (\(x, y) -> (x - 1, y))
        dropShape = tryMove blockers (\(x, y) -> (x, y + 1))
