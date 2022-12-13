module S13 where

import Lib (parseLines, ParserT)
import qualified Text.Parsec as P
import Data.List (sort, elemIndex)
import Data.Maybe (fromJust)

data Packet = PInt Int | PList [Packet] deriving (Show, Eq)

instance Ord Packet where
  compare (PInt a) (PInt b) = compare a b
  compare (PList as) (PList bs) = compare as bs
  compare a@(PInt _) b = compare (PList [a]) b
  compare a b@(PInt _) = compare a (PList [b])

main :: String -> IO ()
main input = do
  let (Right pInput) = P.parse parse "" input

  print $ sum $ zipWith (\i (a, b) -> i * fromEnum (a < b)) [1..] pInput -- part 1

  let p2 = PList [PList [PInt 2]]
  let p6 = PList [PList [PInt 6]]
  let allPackets = sort $ p2 : p6 : concatMap (\(a, b) -> [a, b]) pInput
  print $ (1 + fromJust (elemIndex p2 allPackets)) * (1 + fromJust (elemIndex p6 allPackets))

parse :: ParserT [(Packet, Packet)]
parse = P.sepBy parsePair $ P.char '\n'
  where
    parsePair :: ParserT (Packet, Packet)
    parsePair = (,) <$> parseList <*> parseList
    parseList :: ParserT Packet
    parseList = parseData <* P.char '\n'
    parseData :: ParserT Packet
    parseData = (PInt . read <$> P.many1 P.digit) P.<|> do
      P.char '['
      a <- P.sepBy parseData $ P.char ','
      P.char ']'
      return $ PList a
