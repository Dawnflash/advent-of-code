module S6 where

import qualified Data.Set as S
import qualified Data.Vector as V

main :: String -> IO ()
main input = do
  let sInput = head $ lines input
  print $ detectMarker 4 sInput -- part 1
  print $ detectMarker 14 sInput -- part 2

detectMarker :: Int -> String -> Int
detectMarker n a = let (h, t) = splitAt n a in detectMarker' 0 (V.fromList h) t
  where
    detectMarker' acc buf dat
      | length (S.fromList $ V.toList buf) == n = acc + n
      | otherwise = detectMarker' (succ acc) (V.snoc (V.tail buf) $ head dat) (tail dat)
