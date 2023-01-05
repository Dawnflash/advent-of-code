module S24 where

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import Data.List (elemIndex, find, foldl')
import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import qualified Data.PQueue.Prio.Min as PQ
import qualified Data.Vector as V
import Lib
import qualified Text.Parsec as P

data Spot = Wall | Free | Blizz [Direction2D] deriving (Eq)

instance Show Spot where
  show Wall = "#"
  show Free = "."
  show (Blizz [d]) = show d
  show (Blizz ds) = show $ length ds

type Field = V.Vector Spot -- spots with width and height

type Fields = (V.Vector (V.Vector Spot), (Int, Int))

main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input
  let fElems = V.fromList $ concat pInput
  let dims = (length $ head pInput, length pInput)
  let start = fromJust $ V.elemIndex Free fElems
  let end = V.length fElems - fromJust (V.elemIndex Free $ V.reverse fElems) - 1
  let fields = genFields fElems dims

  -- print dims
  -- print (start, end)
  -- print (fElems V.! start, fElems V.! end)
  -- print $ V.length fElems
  -- printFields fields $ 18 `mod` fieldMod dims

  print $ pathIter (start, end) fields 1 -- round 1
  print $ pathIter (start, end) fields 3 -- round 2

pathIter :: (Int, Int) -> Fields -> Int -> Int
pathIter pts fs i = pathIter' pts fs i 0
  where
    pathIter' _ _ 0 _ = 0
    pathIter' (a, b) fs i off = let res = astar (a, b) fs off in res + pathIter' (b, a) fs (pred i) (res + off)

astar :: (Int, Int) -> Fields -> Int -> Int
astar (start, end) fields@(fs, dims@(w, _)) fieldOffset =
  astar' (PQ.singleton (h start) start) (HM.singleton (start, 0) 0) HS.empty
  where
    pEnd = point2DFromInt w end
    h = distance2DManhattan pEnd . point2DFromInt w -- heuristic
    g fi i = fi - h i -- g-score (f-score with the heuristic subtracted)
    fmod = fieldMod dims
    -- g-scores and closed maps use a dual index (spatial, temporal-cyclic) as the graph has a temporal dimension
    astar' :: PQ.MinPQueue Int Int -> HM.HashMap (Int, Int) Int -> HS.HashSet (Int, Int) -> Int
    astar' q gscores closed
      | cur == end = curF
      | otherwise = astar' q'' gscores' closed'
      where
        closed' = HS.insert (cur, curG `mod` fmod) closed -- close this vertex (in space and cyclic-time)
        ((curF, cur), q') = PQ.deleteFindMin q -- calls error if empty but a path must exist
        curG = g curF cur -- current g-value (represents time elapsed)
        nextG = succ curG -- all edges cost 1
        nextGMod = nextG `mod` fmod -- the temporal index of neighbors (cyclic)
        f = fs V.! ((nextG + fieldOffset) `mod` fmod)

        neighs = filter neighFilter [cur, pred cur, succ cur, cur - w, cur + w]
        -- update our prio queue and gscore map
        q'' = foldl' (\m p -> PQ.insert (nextG + h p) p m) q' neighs
        gscores' = foldl' (\m p -> HM.insert (p, nextGMod) nextG m) gscores neighs

        neighFilter p =
          let mg = HM.lookup (p, nextGMod) gscores
           in (p == end || p == start || isInner dims p) -- exclude walls and endpoints
                && (f V.! p == Free) -- exclude blizzards
                && not (HS.member (p, nextGMod) closed) -- exclude closed spots
                && (null mg || nextG < fromJust mg) -- exclude those with better g-score

-- the fields loop with this modulus (-2 per dim for the walls)
fieldMod :: (Int, Int) -> Int
fieldMod (w, h) = lcm (w - 2) (h - 2)

-- build the cyclic field state (with moving blizzards)
genFields :: Field -> Point2D -> Fields
genFields f dims = (V.fromList $ take (fieldMod dims) $ iterate (stepBlizz dims) f, dims)

-- move blizzards by one step
stepBlizz :: (Int, Int) -> Field -> Field
stepBlizz dims@(w, h) f = V.map calcSpot $ V.zip (V.fromList [0 .. V.length f]) f
  where
    calcSpot (i, spot)
      | not $ isInner dims i = spot -- don't touch the boundary
      | null blizzDirs = Free
      | otherwise = Blizz blizzDirs
      where
        blizzDirs = mapMaybe incoming [DirUp, DirDown, DirLeft, DirRight]
        incoming dir
          | Blizz dirs <- f V.! prevI = find (== dir) dirs
          | otherwise = Nothing
          where
            prevI = pointMap w prevP i -- previous index (looped)
            prevP = (\(x, y) -> (loop x w, loop y h)) . step2D (turn dir DirDown) -- previous point (looped)
            loop a d = pred a `mod` (d - 2) + 1 -- loop around the inside of the board

-- for debugging
printFields :: Fields -> Int -> IO ()
printFields (fs, (w, h)) i = pline 0 (pred w)
  where
    f = fs V.! i
    pline :: Int -> Int -> IO ()
    pline s e
      | s >= w * h = return ()
      | otherwise = mapM_ (\i -> putStr $ show (f V.! i)) [s .. e] >> putChar '\n' >> pline (s + w) (e + w)

-- check if we're inside the boundary
isInner :: (Int, Int) -> Int -> Bool
isInner (w, h) i = i > w && i < pred h * w && i `mod` w /= 0 && i `mod` w /= pred w

-- map a point transformation over an integral representation
pointMap :: Int -> (Point2D -> Point2D) -> Int -> Int
pointMap w f = point2DToInt w . f . point2DFromInt w

parseLine :: ParserT [Spot]
parseLine = P.many1 (Free <$ P.char '.' P.<|> Wall <$ P.char '#' P.<|> Blizz <$> dir)
  where
    dir = do
      dir <- DirUp <$ P.char '^' P.<|> DirRight <$ P.char '>' P.<|> DirDown <$ P.char 'v' P.<|> DirLeft <$ P.char '<'
      return [dir]
