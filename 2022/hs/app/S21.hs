module S21 where

import qualified Data.HashMap.Strict as HM
import Data.List (elemIndex, findIndex)
import Lib
import qualified Data.Ratio as R
import qualified Text.Parsec as P

data ValSide = VLeft | VRight deriving (Show)

data Expr = Op Char String String | Val Int | Var | VarOp ValSide Char Int String deriving (Show)

type ExprMap = HM.HashMap String Expr

showExpr :: ExprMap -> String -> String
showExpr em k = case em HM.! k of
  Val i -> show i
  Var -> "x"
  Op o a b -> "(" ++ showExpr em a ++ ") " ++ o : " (" ++ showExpr em b ++ ")"
  VarOp VLeft o i n -> show i ++ " " ++ o : " (" ++ showExpr em n ++ ")"
  VarOp VRight o i n -> "(" ++ showExpr em n ++ ") " ++ o : " " ++ show i


main :: String -> IO ()
main input = do
  let hmap = HM.fromList $ parseLines parseLine $ lines input

  print . val . fst $ eval hmap "root" -- part 1

  let (Op o a b) = hmap HM.! "root"
  -- set root to subtract and "humn" to variable
  let hmap2 = HM.update (const $ Just $ Op '-' a b) "root" $ HM.update (const $ Just Var) "humn" hmap
  let (er, hmap3) = eval hmap2 "root"

  print $ round $ evalR hmap3 0 er -- part 2

  -- putStrLn $ showExpr hmap3 "root"

val :: Expr -> Int
val (Val n) = n
val _ = 0

evalR :: ExprMap -> R.Rational -> Expr -> R.Rational
evalR hm i Var = i
evalR hm i (Val _) = error "Value???"
evalR hm i (Op {}) = error "Op???"
evalR hm i (VarOp side o v next) = evalR' side o (fromIntegral v) next
  where
    n = hm HM.! next
    evalR' _ '+' v next = evalR hm (i - v) n
    evalR' _ '*' v next = evalR hm (i / v) n
    evalR' VRight '-' v next = evalR hm (i + v) n
    evalR' VLeft '-' v next = evalR hm (-i + v) n
    evalR' VRight '/' v next = evalR hm (i * v) n
    evalR' VLeft '/' v next = error "Inverse!"

eval :: ExprMap -> String -> (Expr, ExprMap)
eval hm k
  | (Op o a b) <- e = eval' o a b
  | otherwise = (e, hm)
  where
    e = hm HM.! k
    op '+' = (+)
    op '-' = (-)
    op '*' = (*)
    op '/' = div
    eval' o a b
      | (Val va, Val vb) <- (ea, eb) = (Val $ op o va vb, HM.update (const . Just $ Val $ op o va vb) k hmb)
      | (Val va, _) <- (ea, eb) = (VarOp VLeft o va b, HM.update (const . Just $ VarOp VLeft o va b) k hmb)
      | (_, Val vb) <- (ea, eb) = (VarOp VRight o vb a, HM.update (const . Just $ VarOp VRight o vb a) k hmb)
      | otherwise = (Var, hmb)
      where
        (ea, hma) = eval hm a
        (eb, hmb) = eval hma b

parseLine :: ParserT (String, Expr)
parseLine = do
  name <- P.count 4 P.anyChar
  P.count 2 P.anyChar
  expr <- (Val <$> parseInt) P.<|> parseOp
  return (name, expr)
  where
    parseOp = do
      [arg1, sop, arg2] <- P.sepBy (P.many1 $ P.noneOf " ") P.space
      return $ Op (head sop) arg1 arg2
