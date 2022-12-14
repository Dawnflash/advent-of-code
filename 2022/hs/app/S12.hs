module S12 where

import Lib
import qualified Data.Vector as V
import Data.Char (ord)
import Data.Maybe (fromJust)
import qualified Data.Set as S

-- height, visited
data VertexT = Vertex Int Bool deriving (Show)
-- width height vertices
data GraphT = Graph Int Int (V.Vector VertexT) deriving (Show)


main :: String -> IO ()
main input = do
  let width = length $ head $ lines input
  let height = length $ lines input
  let vInput = V.fromList [ x | x <- input, x /= '\n' ]
  let start = fromJust $ V.elemIndex 'S' vInput
  let end = fromJust $ V.elemIndex 'E' vInput
  let verts = parseChar <$> vInput
  let graph = Graph width height verts

  print $ bfs graph 0 (S.fromList [start]) end -- part 1
  print $ bfsMin graph end -- part 2

parseChar :: Char -> VertexT
parseChar 'S' = parseChar 'a'
parseChar 'E' = parseChar 'z'
parseChar c = Vertex (ord c - ord 'a') False

visitV :: VertexT -> VertexT
visitV (Vertex h _) = Vertex h True

-- graph, width, length, vertex, target, length (shortest)
bfs :: GraphT -> Int -> S.Set Int -> Int -> Maybe Int
bfs graph@(Graph w h verts) len curs target
  | curs == S.empty = Nothing
  | target `elem` curs = Just len
  | otherwise = bfs ngraph (len + 1) neighbors target
  where
    lcurs = S.toList curs
    ngraph = Graph w h $ verts V.// [(cur, visitV (verts V.! cur)) | cur <- lcurs]
    neighbors = foldl1 S.union $ neighs graph <$> lcurs

-- select suitable neighbors (indices)
neighs :: GraphT -> Int -> S.Set Int
neighs (Graph w h verts) cur = S.fromList $ filter suitable $ point2DToInt w <$> filter (checkBounds0 (w, h)) (neighbors2D (point2DFromInt w cur))
  where
    (Vertex curH _) = verts V.! cur
    suitable i = let (Vertex h v) = verts V.! i in not v && h - curH < 2

bfsMin :: GraphT -> Int -> Int
bfsMin graph@(Graph w h verts) target = foldl findMin (w*h) $ zip [0..] $ V.toList verts
  where
    findMin :: Int -> (Int, VertexT) -> Int
    findMin acc (i, Vertex h _)
      | h == 0 = case bfs graph 0 (S.fromList [i]) target of
        Nothing -> acc
        Just a -> min a acc
      | otherwise = acc
