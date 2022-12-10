module S12 where

import Lib (parseLines)
import qualified Text.Parsec as P


main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input

  print pInput

parseLine :: P.Parsec String () String
parseLine = P.many P.anyChar