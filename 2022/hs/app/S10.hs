module S10 where

import Lib
import qualified Text.Parsec as P
import qualified Data.Vector as V

data Instr = Noop | Addx Int deriving (Show)

type State = Int

main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input
  let p1Cycles = [20, 60, 100, 140, 180, 220]
  let cycles = V.fromList $ reverse $ tail $ foldl run [1] pInput

  print $ sum $ signal cycles <$> p1Cycles              -- part 1
  print2D $ foldl draw [] $ zip [0..] $ V.toList cycles -- part 2

parseLine :: ParserT Instr
parseLine = (P.string "noop" >> return Noop) P.<|> (P.string "addx " >> Addx . read <$> P.many P.anyChar)

signal :: V.Vector State -> Int -> Int
signal s i = (s V.! (i - 1)) * i

run :: [State] -> Instr -> [State]
run s@(h:_) Noop = h:s
run s@(h:_) (Addx i) = (h+i):h:s

draw :: [Point2D] -> (Int, State) -> [Point2D]
draw acc (i, s)
  | abs (s - i `mod` 40) < 2 = point2DFromInt 40 i : acc
  | otherwise = acc
