module S23 where

import Lib
import Data.Maybe (catMaybes, mapMaybe)
import qualified Text.Parsec as P
import qualified Data.Set as S
import qualified Data.Map as M

type Elves = S.Set Point2D
type State = (Elves, [Direction2D])


main :: String -> IO ()
main input = do
  let pInput = parseLines parseLine $ lines input
  let elves = S.fromList [(x, y) | (y, line) <- zip [0..] pInput, (x, elf) <- zip [0..] line, elf]
  let state = (elves, [DirUp, DirDown, DirLeft, DirRight])

  -- trace state 1
  print $ boundedVoid $ fst $ iterate (fst . step) state !! 10 -- part 1
  print $ iterLen state 1 -- part 2

  -- print2D $ S.toList $ fst $ iterate (fst . step) state !! 984

-- number of void tiles in a minimal rectangle
boundedVoid :: Elves -> Int
boundedVoid es = area - S.size es
  where
    ((x1, y1), (x2, y2)) = boundaries2D $ S.toList es
    area = succ (x2 - x1) * succ (y2 - y1)


-- find the length of the iteration
iterLen :: State -> Int -> Int
iterLen s@(es, ds) cur
  | not changed = cur
  | otherwise = iterLen ns $ succ cur
  where
    (ns, changed) = step s

-- new state, True if changed
step :: State -> (State, Bool)
step s@(elves, ds@(hds:tds)) = ((elves', tds ++ [hds]), changed)
  where
    (elves', changed) = uniq props
    -- proposal: [elf]
    props :: M.Map Point2D [Point2D]
    props = foldl propFold M.empty elves
      where
        propFold m p = M.alter altF prop m
          where
            prop = propose s p
            altF Nothing = Just [p]
            altF (Just ps) = Just $ p:ps
    -- dedup proposals, identify change
    uniq :: M.Map Point2D [Point2D] -> (Elves, Bool)
    uniq = M.foldlWithKey' catProps (S.empty, False)
      where
        catProps (s, t) prop [orig] = (S.insert prop s, t || prop /= orig)
        catProps (s, t) _ origs = (S.union s $ S.fromList origs, t)

-- proposal from one elf
propose :: State -> Point2D -> Point2D
propose (es, ds) p
  | null $ catMaybes adjs = p -- isolated = stay
  | (h:_) <- mapMaybe proposeDir ds = h -- can move = move
  | otherwise = p -- cannot move = stay
  where
    adjs@[nw,n,ne,e,se,s,sw,w] = adjacent es p
    targets DirUp = [nw, n, ne]
    targets DirRight = [ne, e, se]
    targets DirDown = [se, s, sw]
    targets DirLeft = [sw, w, nw]
    proposeDir dir
      | null $ catMaybes $ targets dir = Just $ step2D dir p
      | otherwise = Nothing

-- get adjacent elves (NW -> W)
adjacent :: Elves -> Point2D -> [Maybe Point2D]
adjacent es p = check <$> adjacent2D p
  where
    check e
      | e `S.member` es = Just e
      | otherwise = Nothing

parseLine :: ParserT [Bool]
parseLine = P.many1 $ True <$ P.char '#' P.<|> False <$ P.char '.'

-- for debugging
trace :: State -> Int -> IO ()
trace s@(es, ds) cur
  | not changed = return ()
  | otherwise = do
    putStrLn $ "---- " ++ show cur ++ " ----" ++ " | " ++ show (S.size es) ++ " | " ++ show ds ++ " | " ++ show es
    -- mapM_ (\e -> putStrLn $ show e ++ " proposes " ++ show (propose s e)) es
    print2D $ S.toList es
    trace ns (succ cur)
  where
    (ns, changed) = step s