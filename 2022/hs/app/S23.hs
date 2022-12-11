module S23 where

import Lib (parseLines, ParserT)
import qualified Text.Parsec as P


main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input

  print pInput

parseLine :: ParserT String
parseLine = P.many P.anyChar