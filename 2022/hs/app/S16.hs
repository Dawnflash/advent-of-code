{-# LANGUAGE TupleSections #-}
module S16 where

import Lib
import qualified Data.Vector as V
import qualified Text.Parsec as P
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Data.Char (ord, chr)
import qualified Control.Parallel.Strategies as ParS

-- flow rate, destinations (weighted)
type Node = (Int, M.Map Int Int)
-- node carrier with indices (in a set)
type Graph = (V.Vector Node, S.Set Int)

data MoveT = Move (Int, Int) | Open | Noop deriving (Eq, Show)

main :: String -> IO ()
main input = do
  let iNodes = parseLines parseLine $ lines input
  let graph@(nodes, indices) = (V.fromList (replicate (26^2) (0, M.empty)) V.// iNodes, S.fromList (fst <$> iNodes)) :: Graph
  let start = valveId "AA"

  let pgraph@(v, is) = foldl (prune start) graph indices

  print $ S.map (\x -> (valveIdInv x, v V.! x)) is
  print $ S.map valveIdInv is
  print $ v V.! valveId "AA"

  let p1 = maxpath (S.size is) 30 start start pgraph -- part 1
  print p1
  let bound = 2800 -- p1 is too low?
  print $ maxpath2 p1 26 start graph -- part 2, use p1 result as a branch boundary

-- remove all 0-nodes
prune :: Int -> Graph -> Int -> Graph
prune start graph@(nodes, is) nid
  | nid == start = graph -- don't prune the starting node
  | power /= 0 = graph -- don't prune actionable nodes
  | otherwise = let (ns, is) = foldl prune' graph (M.toList adj) in (ns, S.delete nid is) -- finish pruning: drop node from node set
  where
    (power, adj) = nodes V.! nid
    -- add/update routes to other adjs
    prune' :: Graph -> (Int, Int) -> Graph
    prune' (ns, is) (ai, aw) = (ns V.// [(ai, (ap, naadj))], is)
      where
        (ap,  aadj) = ns V.! ai
        adjRest = S.delete ai $ M.keysSet adj -- targets
        naadj = M.delete nid $ foldl link aadj adjRest
        -- link a new adjacency or update an existing one
        link :: M.Map Int Int -> Int -> M.Map Int Int
        link m i
          | M.notMember i m = M.insert i newWt m -- add new adjacency
          | otherwise = M.adjust (`min` newWt) i m
          where
            newWt = aw + adj M.! i -- pruned node has link to this node

maxSafe :: Int -> [Int] -> Int
maxSafe d [] = d
maxSafe _ a = maximum a

-- dumb combinatorial explosion (cringe)
maxpath :: Int -> Int -> Int -> Int -> Graph -> Int
maxpath nclosed rem prev cur graph@(verts, is)
  | rem <= 1 || nclosed <= 0 = 0 -- nothing more to gain!
  | otherwise = maxSafe 0 $ evalMoves moves
  where
    execMove Open = pwr * (rem - 1) + maxpath (nclosed - 1) (rem - 1) cur cur (verts V.// [(cur, (0, adj))], is) -- close vertex
    execMove (Move (dest, w)) = maxpath nclosed (rem - w) cur dest graph
    evalMoves ms
      | rem > 10 = ParS.parMap ParS.rdeepseq execMove ms
      | otherwise = map execMove ms
    (pwr, adj) = verts V.! cur
    _moves = if rem > 2 then M.foldrWithKey folder [] $ M.delete prev adj else []
    moves = if pwr > 0 then Open : _moves else _moves
    folder k w acc = Move (k, w) : acc -- don't backtrack

-- 2785 (calculated) < 2800 < x < 3000
-- dumb combinatorial explosion (cringe, with 2 actors, bounded by 1-actor optimum)
maxpath2 :: Int -> Int -> Int -> Graph -> Int
maxpath2 bound timeout start (verts, is) = maxpath2' 0 (S.size initOpens) timeout (start, start) (start, start) verts initOpens
  where
    -- sorted by powers, which are apparently unique (stimke)
    initOpens = S.fromList [pow | k <- S.toList is, let pow = fst (verts V.! k), pow /= 0]
    maxpath2' :: Int -> Int -> Int -> (Int, Int) -> (Int, Int) -> V.Vector Node -> S.Set Int -> Int
    maxpath2' score nclosed rem (prev1, prev2) (cur1, cur2) verts opens
      | rem <= 1 || nclosed <= 0 || theorMax <= bound = score -- nothing more to gain!
      | otherwise = maxSafe score $ evalMoves moves1 moves2
      where
        execMove Open Open = maxpath2' (score + pwr1 * (rem - 1) + pwr2 * (rem - 1)) (nclosed - 2) (rem - 1) (cur1, cur2) (cur1, cur2) (verts V.// [(cur1, (0, adj1)),(cur2, (0, adj2))]) $ S.delete pwr1 $ S.delete pwr2 opens
        execMove Open (Move (dest, _)) = maxpath2' (score + pwr1 * (rem - 1)) (nclosed - 1) (rem - 1) (cur1, cur2) (cur1, dest) (verts V.// [(cur1, (0, adj1))]) $ S.delete pwr1 opens
        execMove (Move (dest, _)) Open = maxpath2' (score + pwr2 * (rem - 1)) (nclosed - 1) (rem - 1) (cur1, cur2) (dest, cur2) (verts V.// [(cur2, (0, adj2))]) $ S.delete pwr2 opens
        execMove (Move (dest1, _)) (Move (dest2, _)) = maxpath2' score nclosed (rem - 1) (cur1, cur2) (dest1, dest2) verts opens
        evalMoves ms1 ms2
          | rem > 10 = ParS.parMap ParS.rdeepseq (uncurry execMove) ls
          | otherwise = map (uncurry execMove) ls
          where ls = [(m1, m2) | m1 <- moves1, m2 <- moves2] -- cartesian product of moves
        (pwr1, adj1) = verts V.! cur1
        (pwr2, adj2) = verts V.! cur2
        moves1 = mkMoves True pwr1 prev1 adj1
        moves2 = mkMoves (cur1 /= cur2) pwr2 prev2 adj2
        folder k w acc = Move (k, w) : acc -- don't backtrack
        mkMoves openCond pwr prev adj
          | pwr > 0 && openCond = Open : moves
          | null moves = [Move (prev, 1)]
          | otherwise = moves
          where
            moves = if rem < 3 then [] else M.foldrWithKey folder [] $ M.delete prev adj
        theorMax = score + theorMaxCalc rem (S.toDescList opens)
        theorMaxCalc r (p1:p2:t) = (r - 1) * p1 + (r - 1) * p2 + theorMaxCalc (r - 2) t
        theorMaxCalc r [p] = (r - 1) * p
        theorMaxCalc _ _ = 0

parseLine :: ParserT (Int, Node)
parseLine = do
  P.string "Valve "
  vid <- P.count 2 P.anyChar
  P.string " has flow rate="
  rate <- parseInt
  P.string "; tunnel" >> P.optional (P.char 's')
  P.string " lead" >> P.optional (P.char 's')
  P.string " to valve" >> P.optional (P.char 's')
  P.char ' '
  tids <- P.sepBy (P.count 2 P.anyChar) (P.string ", ")
  return (valveId vid, (rate, M.fromList $ (, 1) . valveId <$> tids))


valveId :: String -> Int
valveId [a, b] = let oa = ord 'A' in (ord a - oa) * 26 + ord b - oa

valveIdInv :: Int -> String
valveIdInv i = let (a, b) = divMod i 26 in [chr (a + ord 'A'), chr (b + ord 'A')]
