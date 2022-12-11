module S11 where

import Lib
import qualified Text.Parsec as P
import Data.Either (fromRight)

type WorryL = Int
type MonkeyID = Int
data MonkeyOp = OpConst WorryL | OpVar | OpAdd | OpMult
data MonkeyT = Monkey [WorryL] (WorryL -> WorryL) (WorryL -> MonkeyID)

instance Show MonkeyT where
  show (Monkey l _ _) = "Monkey " ++ show l


main :: String -> IO ()
main input = do
  let pInput = fromRight [] $ P.parse parseMonkeys "" input

  print pInput

parseMonkeys :: P.Parsec String () [MonkeyT]
parseMonkeys = P.sepBy parseMonkey $ P.char '\n'
  where
    parseMonkey :: P.Parsec String () MonkeyT
    parseMonkey = do
      P.string "Monkey "
      P.many P.digit -- discard the monkey ID
      P.string ":\n"
      items <- parseItems
      op <- parseInspectOp
      test <- parseTest
      testT <- parseTestDecision
      testF <- parseTestDecision
      return $ Monkey items op $ (\t -> if t then testT else testF) . test

    parseItems :: P.Parsec String () [WorryL]
    parseItems = P.string "  Starting items: " >> P.sepBy (read <$> P.many P.digit) (P.string ", ") <* P.char '\n'

    parseInspectOp :: P.Parsec String () (WorryL -> WorryL)
    parseInspectOp = do
      P.string "  Operation: new = old "
      op <- parseInspectOpElem
      P.space
      parseInspectOp' op <$> parseInspectOpElem <* P.char '\n'
      where
        parseInspectOp' OpAdd OpVar = (* 2)
        parseInspectOp' OpMult OpVar = \x -> x * x
        parseInspectOp' OpAdd (OpConst a) = (+ a)
        parseInspectOp' OpMult (OpConst a) = (* a)

    parseInspectOpElem :: P.Parsec String () MonkeyOp
    parseInspectOpElem = (P.char '*' >> return OpMult)
      P.<|> (P.char '+' >> return OpAdd)
      P.<|> (P.string "old" >> return OpVar)
      P.<|> (OpConst . read <$> P.many P.digit)

    parseTest :: P.Parsec String () (WorryL -> Bool)
    parseTest = P.string "  Test: divisible by " >> (\y x -> x `mod` y == 0) . read <$> P.many P.digit <* P.char '\n'

    parseTestDecision :: P.Parsec String () WorryL
    parseTestDecision = do
      P.string "    If "
      P.string "true" P.<|> P.string "false"
      P.string ": throw to monkey "
      read <$> P.many P.digit <* P.char '\n'
