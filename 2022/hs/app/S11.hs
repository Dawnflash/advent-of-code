module S11 where

import Lib
import qualified Text.Parsec as P
import qualified Data.Vector as V
import qualified Data.List as L
import Data.Either (fromRight)

type WorryL = Int
type ActivityL = Int
type MonkeyID = Int
type MonkeyModulus = Int
data MonkeyOp = OpConst WorryL | OpVar | OpAdd | OpMult
data MonkeyT = Monkey ActivityL (V.Vector WorryL) MonkeyModulus (WorryL -> WorryL) (WorryL -> MonkeyID)

instance Show MonkeyT where
  show (Monkey i l _ _ _) = "Monkey <" ++ show i ++ "> " ++ show l

monkeyActivity :: MonkeyT -> ActivityL
monkeyActivity (Monkey a _ _ _ _) = a

monkeyModulus :: MonkeyT -> MonkeyModulus
monkeyModulus (Monkey _ _ d _ _) = d


main :: String -> IO ()
main input = do
  let monkeys = fromRight V.empty $ P.parse parseMonkeys "" input
  let modulus = foldl lcm 1 $ monkeyModulus <$> monkeys

  let i1 = iterate (run 3 modulus) monkeys !! 20
  let i2 = iterate (run 1 modulus) monkeys !! 10000

  print $ monkeyBusiness i1 -- part 1
  print $ monkeyBusiness i2 -- part 2

monkeyBusiness :: V.Vector MonkeyT -> ActivityL
monkeyBusiness = product . take 2 . reverse . L.sort . fmap monkeyActivity . V.toList

-- one iteration (relief = 1 for part 2)
run :: Int -> MonkeyModulus -> V.Vector MonkeyT -> V.Vector MonkeyT
run relief modulus = run' 0
  where
    run' n monkeys
      | n >= V.length monkeys = monkeys
      | otherwise = run' (n + 1) $ V.unsafeUpd (foldl runItem monkeys items) [(n, Monkey (a + V.length items) V.empty d wf tf)]
      where
        (Monkey a items d wf tf) = monkeys V.! n
        runItem :: V.Vector MonkeyT -> WorryL -> V.Vector MonkeyT
        runItem monkeys w = V.unsafeUpd monkeys [(tmi, Monkey a (V.snoc titems nw) d twf ttf)]
          where
            nw = (wf w `div` relief) `mod` modulus
            tmi = tf nw
            (Monkey a titems d twf ttf) = monkeys V.! tmi


parseMonkeys :: ParserT (V.Vector MonkeyT)
parseMonkeys = V.fromList <$> P.sepBy parseMonkey (P.char '\n')
  where
    parseMonkey :: ParserT MonkeyT
    parseMonkey = do
      P.string "Monkey "
      P.many P.digit -- discard the monkey ID
      P.string ":\n"
      items <- V.fromList <$> parseItems
      op <- parseInspectOp
      div <- parseDivisor
      testT <- parseTestDecision
      testF <- parseTestDecision
      return $ Monkey 0 items div op (\w -> if w `mod` div == 0 then testT else testF)

    parseItems :: ParserT [WorryL]
    parseItems = P.string "  Starting items: " >> P.sepBy (read <$> P.many P.digit) (P.string ", ") <* P.char '\n'

    parseInspectOp :: ParserT (WorryL -> WorryL)
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

    parseInspectOpElem :: ParserT MonkeyOp
    parseInspectOpElem = (P.char '*' >> return OpMult)
      P.<|> (P.char '+' >> return OpAdd)
      P.<|> (P.string "old" >> return OpVar)
      P.<|> (OpConst . read <$> P.many P.digit)

    parseDivisor :: ParserT MonkeyModulus
    parseDivisor = P.string "  Test: divisible by " >> read <$> P.many P.digit <* P.char '\n'

    parseTestDecision :: ParserT WorryL
    parseTestDecision = do
      P.string "    If "
      P.string "true" P.<|> P.string "false"
      P.string ": throw to monkey "
      read <$> P.many P.digit <* P.char '\n'
