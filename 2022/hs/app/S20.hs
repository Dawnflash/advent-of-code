module S20 where

import Data.Maybe (fromJust)
import qualified Data.Sequence as S
import Lib
import qualified Text.Parsec as P

-- number, index
type Elem = (Int, Int)

type State = S.Seq Elem

main :: String -> IO ()
main input = do
  let state = S.fromList $ zip (parseLines parseInt $ lines input) [0 ..]
  let blen = S.length state

  -- mapM_ (print . fmap fst) $ scanl shift state [0 .. blen - 1]
  print $ sum $ map (evalAt $ mix state) [1000, 2000, 3000] -- part 1
  let state2 = S.fromList $ zip (map (811589153 *) $ parseLines parseInt $ lines input) [0 ..]

  print $ sum $ map (evalAt $ iterate mix state2 !! 10) [1000, 2000, 3000] -- part 2

mix :: State -> State
mix s = foldl shift s [0 .. S.length s - 1]

shift :: State -> Int -> State
shift s i = S.insertAt dstIx src $ S.deleteAt srcIx s
  where
    srcIx = fromJust $ S.findIndexL ((== i) . snd) s
    src@(srcVal, _) = fromJust $ s S.!? srcIx
    dstIx = (srcVal + srcIx) `mod` (S.length s - 1) -- during insertion the sequence is shorter by 1

evalAt :: State -> Int -> Int
evalAt s i = fst . fromJust $ s S.!? ((i + zi) `mod` S.length s)
  where
    zi = fromJust $ S.findIndexL ((== 0) . fst) s
