module S18 where

import qualified Control.Monad as CM
import qualified Data.List as L
import qualified Data.Set as S
import Lib
import qualified Text.Parsec as P

main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input

  print $ sum $ map (surfNaive pInput) pInput -- part 1
  let bounds = [(minimum vi, maximum vi) | i <- [0 .. 2], let vi = map (!! i) pInput]
  print $ sum $ map (surfProper bounds (S.fromList pInput)) pInput -- part 2

parseLine :: ParserT Point
parseLine = P.sepBy parseInt $ P.char ','

surfNaive :: [Point] -> Point -> Int
surfNaive obj p = 6 - sum (map (fromEnum . isAdjacentND p) obj)

surfProper :: [(Int, Int)] -> S.Set Point -> Point -> Int
surfProper bounds obj = length . filter (not . isInner bounds obj) . neighborsND

isInner :: [(Int, Int)] -> S.Set Point -> Point -> Bool
isInner bounds obj p = S.member p obj || bfs obj (S.singleton p)
  where
    bfs :: S.Set Point -> S.Set Point -> Bool
    bfs blockers pts
      | (Just next) <- mnext = S.null next || bfs (S.union blockers pts) next
      | otherwise = False
      where
        mnext = CM.foldM union S.empty pts
        union :: S.Set Point -> Point -> Maybe (S.Set Point)
        union acc p
          | isBoundedExclND bounds p = Just $ S.union acc $ S.fromList $ filter (`S.notMember` blockers) $ neighborsND p
          | otherwise = Nothing
