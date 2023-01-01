module S22 where

import Lib
import qualified Text.Parsec as P
import qualified Data.Vector.Unboxed as V
import qualified Data.Bit as B
import qualified Data.HashMap.Strict as HM
import Data.List (findIndices)
import Data.Maybe (fromJust, catMaybes, mapMaybe)

data Instr = IRight | ILeft | IGo Int deriving (Show)

type Bound = (Int, Int) -- start/end of row or column
type Board = (V.Vector B.Bit, Int, Cube, V.Vector Bound, V.Vector Bound) -- board, width, face size, row bounds, col bounds
type Pos = (Point2D, Direction2D) -- 2D coordinates, internal board pointer, direction
type State = (Board, [Instr], Pos)

-- part 2
type Face = (Point2D, Direction2D) -- connecting face and side (Nothing if unknown)
type FaceMap = HM.HashMap Point2D [Face] -- faces indexed by their segment (x, y) address
type Cube = (FaceMap, Int) -- faces, side length


main :: String -> IO ()
main input = do
  let (Right (board, instrs)) = P.parse parse "" input
  let state = (board, instrs, initPos board)

  let (_, _, pos) = run nextPos state
  print $ password pos -- part 1

  let (_, _, pos2) = run nextPos2 state
  print $ password pos2 -- part 2

run :: (Board -> Pos -> Pos) -> State -> State
run _ s@(_, [], _) = s
run f (board, ih:it, pos@(pt, dir)) = case ih of
  IRight -> run f (board, it, (pt, turn dir DirRight))
  ILeft -> run f (board, it, (pt, turn dir DirLeft))
  IGo 0 -> run f (board, it, pos)
  IGo n -> run f (board, IGo (pred n):it, f board pos)

-- determine where to go given a position (Nothing if blocked)
nextPos :: Board -> Pos -> Pos
nextPos (bs, w, _, rbs, cbs) pos@(p@(x, y), dir)
  | B.unBit $ bs V.! point2DToInt w p' = pos -- stay
  | otherwise = (p', dir) -- go
  where
    (rbeg, rend) = rbs V.! y
    (cbeg, cend) = cbs V.! x
    p' = next' dir
    next' DirRight
      | succ x < rend = (succ x, y)
      | otherwise = (rbeg, y)
    next' DirLeft
      | x > rbeg = (pred x, y)
      | otherwise = (pred rend, y)
    next' DirDown
      | succ y < cend = (x, succ y)
      | otherwise = (x, cbeg)
    next' DirUp
      | y > cbeg = (x, pred y)
      | otherwise = (x, pred cend)

-- determine where to go given a position (Nothing if blocked)
-- using cube topology
nextPos2 :: Board -> Pos -> Pos
nextPos2 (bs, w, (cube, csize), rbs, cbs) pos@(p@(x, y), dir)
  | B.unBit $ bs V.! point2DToInt w (fst nxt)  = pos -- stay
  | otherwise = nxt -- go
  where
    (rbeg, rend) = rbs V.! y
    (cbeg, cend) = cbs V.! x
    nxt = next' dir
    next' DirRight
      | succ x < rend = ((succ x, y), DirRight)
      | otherwise = jump
    next' DirLeft
      | x > rbeg = ((pred x, y), DirLeft)
      | otherwise = jump
    next' DirDown
      | succ y < cend = ((x, succ y), DirDown)
      | otherwise = jump
    next' DirUp
      | y > cbeg = ((x, pred y), DirUp)
      | otherwise = jump
    -- cube movement functions
    (xd, xrem) = x `divMod` csize
    (yd, yrem) = y `divMod` csize
    (srcFace, srcOffset) = ((xd, yd), (xrem, yrem))
    (dstFace, dstFaceDir) = (cube HM.! srcFace) !! fromEnum dir
    dstDir = turn dstFaceDir DirDown -- flipped destination face direction - we'll go this way
    dstFaceAdj = step2D dstFaceDir dstFace -- "naturally adjacent" to the destination face
    -- translate to the correct adjacent face, rotate and move one step
    jump = (step2D dstDir (embed (iterate rotate (xrem, yrem) !! rotTimes)), dstDir)
    embed = map2D (+) (map2D (*) (csize, csize) dstFaceAdj) -- face offset -> board point
    rotTimes = fromEnum $ angle dir dstFaceDir
    rotate (x, y) = (pred csize - y, x)

-- angle between two faces
angle :: Direction2D -> Direction2D -> Direction2D
angle src dst = turnInv (turn dst DirDown) src

-- convert position to a final score
password :: Pos -> Int
password ((x, y), dir) = succ x * 4 + succ y * 1000 + case dir of
  DirRight -> 0
  DirDown -> 1
  DirLeft -> 2
  DirUp -> 3

-- initial position - top left-most unblocked space
initPos :: Board -> Pos
initPos (bs, _, _, rbs, _) = ((fromJust $ V.findIndex (not . B.unBit) bs, 0), DirRight)

-- generate the cube model with fully computed edges
genCube :: Point2D -> V.Vector Bound -> V.Vector Bound -> Cube
genCube (w, h) rbs cbs = (finish initCube, faceSize)
  where
    faceSize = gcd w h
    cube = HM.fromList [((x, y), []) | x <- [0..w `div` faceSize - 1], y <- [0..h `div` faceSize - 1], hit (x, y)]
    cubeMap f cube = HM.mapWithKey (f cube) cube -- map with access to the full cube
    initCube = cubeMap initAdj cube
    hit (x, y) = hit' x' rb && hit' y' cb -- check that face is within bounds
      where
        x' = x * faceSize
        y' = y * faceSize
        rb = rbs V.! y'
        cb = cbs V.! x'
        hit' i (a, b) = a <= i && i < b
    initAdj cube k _ = map adj [DirUp ..]
      -- connect to all adjacent faces (use the flipped side (eg. right -> left))
      where adj dir = let nk = step2D dir k in (nk, turn dir DirDown) <$ (cube HM.!? nk)
    -- check that all edges have been filled
    isComplete = HM.foldl (\acc v -> acc && length (catMaybes v) == 4) True
    -- keep filling edges until the cube is complete
    finish cube
      | isComplete cube = fmap catMaybes cube
      | otherwise = finish $ cubeMap fill cube
    -- fill in missing edges by folding at right angles
    -- (A -> B -> C :: A -> C where A/B and B/C are at a right angle)
    fill cube k edges = zipWith fillSide [DirUp ..] edges
      where
        fillSide _ e@(Just _) = e -- don't touch existing edges
        fillSide side _ = case mapMaybe detect [turn side DirRight, turn side DirLeft] of
          a:_ -> Just a
          _ -> Nothing
          where
            detect sideAsrc = case edges !! fromEnum sideAsrc of
              Nothing -> Nothing
              Just (faceB, faceBdst) -> let
                  angle1 = angle sideAsrc faceBdst -- angle between A/B
                  faceBsrc = turn angle1 side -- source between B/C
                in case (cube HM.! faceB) !! fromEnum faceBsrc of
                  Nothing -> Nothing
                  Just (faceC, faceCdst) -> let
                      angle2 = turn angle1 $ angle faceBsrc faceCdst -- angle between A/C (compound)
                      faceCsrc = turn angle2 $ turn sideAsrc DirDown -- link side at C
                    in Just (faceC, faceCsrc)

parse :: ParserT (Board, [Instr])
parse = (,) <$> parseBoard <*> parseInstr
  where
    parseInstr = P.many1 (IGo <$> parseInt P.<|> ILeft <$ P.char 'L' P.<|> IRight <$ P.char 'R') <* P.endOfLine
    parseBoard = do
      (lines, rowBounds) <- unzip <$> P.endBy1 parseBoardLine P.endOfLine
      let w = maximum $ snd <$> rowBounds
      let h = length rowBounds
      let (rbs, cbs) = (V.fromList rowBounds, V.fromList $ colBounds rowBounds 0)
      -- pad map with blockers (left and right)
      let lines' = zipWith (\ls (b, e) -> replicate b (B.Bit True) ++ ls ++ replicate (w - e) (B.Bit True)) lines rowBounds
      P.endOfLine
      return (V.fromList $ concat lines', w, genCube (w, h) rbs cbs, rbs, cbs)
    parseBoardLine = do
      as <- P.many $ P.char ' '
      l <- P.many1 (B.Bit False <$ P.char '.' P.<|> B.Bit True <$ P.char '#')
      return (l, (length as, length as + length l))
    colBounds rbs i
      | null ixs = []
      | otherwise = (minimum ixs, maximum ixs + 1) : colBounds rbs (succ i)
      where
        ixs = findIndices hit rbs
        hit (a, b) = a <= i && i < b
