module S5 where

import qualified Data.Vector as V
import Lib
import qualified Text.Parsec as P

data MoveT = Move Int Int Int

main :: String -> IO ()
main input = do
  let inputLines = lines input
  let [sCrates', sMoves] = V.fromList <$> split "" inputLines
  let sCrates = V.tail $ V.reverse sCrates' -- flip and drop the annotations
  let moves = parseMove <$> sMoves
  let crates = parseCrates sCrates

  print $ head <$> foldl moveCrates crates moves -- part 1
  print $ head <$> foldl moveCratesN crates moves -- part 2

-- each crate slot is a string (char stack)
parseCrates :: V.Vector String -> V.Vector String
parseCrates crates = foldl parseCrates' (V.replicate nslots "") crates
  where
    nslots = (length (V.head crates) + 1) `div` 4

    parseCrates' :: V.Vector String -> String -> V.Vector String
    parseCrates' acc s = case P.parse parseSlots "" s of
      Left e -> error $ show e
      Right ps -> V.zipWith maybeAdd ps acc

    maybeAdd (Just a) b = a : b
    maybeAdd _ b = b

    parseSlots :: P.Parsec String () (V.Vector (Maybe Char))
    parseSlots = V.fromList <$> P.sepBy (pVoid P.<|> pCrate) P.space
      where
        pVoid = P.count 3 P.space >> return Nothing
        pCrate = P.char '[' *> (Just <$> P.letter) <* P.char ']'

parseMove :: String -> MoveT
parseMove x = let [_, n, _, from, _, to] = words x in Move (read n) (read from - 1) (read to - 1)

-- crates, move -> new crates
moveCrates :: V.Vector String -> MoveT -> V.Vector String
moveCrates crates (Move n from to) = iterate (moveCrate from to) crates !! n
  where
    moveCrate from to crates =
      let (h : t) = crates V.! from
       in crates V.// [(from, t), (to, h : crates V.! to)]

-- part 2
moveCratesN :: V.Vector String -> MoveT -> V.Vector String
moveCratesN crates (Move n from to) =
  let (h, t) = splitAt n $ crates V.! from
   in crates V.// [(from, t), (to, h ++ crates V.! to)]
