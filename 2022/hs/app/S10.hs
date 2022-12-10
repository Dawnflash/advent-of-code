module S10 where

import Lib (parseLines)
import qualified Text.Parsec as P

data Instr = Noop | Addx Int deriving (Show)

main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input
  let p1Cycles = [20, 60, 100, 140, 180, 220]

  print pInput

parseLine :: P.Parsec String () Instr
parseLine = (P.string "noop" >> return Noop) P.<|> (P.string "addx " >> Addx . read <$> P.many P.anyChar)
