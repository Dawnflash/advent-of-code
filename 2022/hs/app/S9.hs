module S9 where

import Lib
import qualified Text.Parsec as P
import qualified Data.Set as S
import Data.Either (rights)

type Cmd = (Direction2D, Int)
-- head, tail, crumbs
type State = ([Point2D], S.Set Point2D)

main :: String -> IO ()
main input = do
  let inputLines = lines input
  let pInputLines = rights $ P.parse parseLine "" <$> inputLines

  print $ S.size . snd $ foldl visitFold (replicate 2 (0, 0), S.fromList [(0, 0)]) pInputLines  -- part 1
  print $ S.size . snd $ foldl visitFold (replicate 10 (0, 0), S.fromList [(0, 0)]) pInputLines -- part 2

parseLine :: P.Parsec String () Cmd
parseLine = do
  d <- (P.char 'U' >> return DirUp) P.<|> (P.char 'D' >> return DirDown) P.<|> (P.char 'L' >> return DirLeft) P.<|> (P.char 'R' >> return DirRight)
  P.space
  n <- read <$> P.many P.digit
  return (d, n)

visitFold :: State -> Cmd -> State
visitFold st (_, 0) = st
visitFold st (dir, n) = visitFold (visitStep st dir) (dir, n - 1)

visitStep :: State -> Direction2D -> State
visitStep (h:t, crumbs) dir = (nh:nt, nc)
  where
    nh = step2D dir h
    (nt, nc) = visitStep' (t, crumbs) h nh

    -- next position of a tail given a new/old head position
    tailStep :: Point2D -> Point2D -> Point2D -> Point2D
    tailStep t oh nh
      | isNeighbor2D nh t = t               -- no tail motion
      | xdh == 0 || ydh == 0 = oh           -- head moving straight: follow head
      | xd == 0 || yd == 0 = map2D (+) t hd -- head moving diagonally (v-case): move towards head
      | otherwise = map2D (+) t dh          -- head moving diagonally: copy head's move
      where
        hd = (xd `div` 2, yd `div` 2)
        d@(xd, yd) = map2D (-) nh t
        dh@(xdh, ydh) = map2D (-) nh oh

    visitStep' :: State -> Point2D -> Point2D -> State
    -- end condition - record new tail position if moved
    visitStep' ([], crumbs) oph nph
      | oph == nph = ([], crumbs)
      | otherwise = ([], S.insert nph crumbs)
    visitStep' (h:t, crumbs) oph nph = (nh:nt, nc)
      where
        nh = tailStep h oph nph
        (nt, nc) = visitStep' (t, crumbs) h nh
