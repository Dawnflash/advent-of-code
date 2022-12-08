module S7 where

import qualified Data.Tree as T
import qualified Text.Parsec as P
import Data.Either (rights)

-- name and size, don't store files individually yet
type Dir = T.Tree (String, Int)

data Cmd = CmdLs | CmdCdUp | CmdCd String | ODir String | OFile String Int deriving (Show)

main :: String -> IO ()
main input = do
  let inputLines = tail $ lines input
  let pInputLines = rights $ P.parse parseLine "" <$> inputLines
  let dirTree = makeDir (newDir "/") pInputLines
  let sizeTotal = snd $ T.rootLabel dirTree
  let sizeNeeded = 30000000 - 70000000 + sizeTotal
  -- print pInputLines
  -- putStrLn $ T.drawTree $ show <$> dirTree
  print $ foldl stage1Add 0 dirTree -- part 1
  print $ foldl (stage2Min sizeNeeded) sizeTotal dirTree -- part 2

stage1Add :: Int -> (String, Int) -> Int
stage1Add acc (_, ds)
  | ds < 100000 = acc + ds
  | otherwise = acc

stage2Min :: Int -> Int -> (String, Int) -> Int
stage2Min needed acc (_, ds)
  | ds < acc && ds >= needed = ds
  | otherwise = acc

newDir :: String -> Dir
newDir name = T.Node (name, 0) []

addDir :: Dir -> Dir -> Dir
addDir dir@(T.Node (_, dsz1) _) (T.Node (dn, dsz2) sf) = T.Node (dn, dsz1 + dsz2) $ dir : sf

resizeDir :: Int -> Dir -> Dir
resizeDir n (T.Node (dn, dsz) sf) = T.Node (dn, dsz + n) sf

makeDir :: Dir -> [Cmd] -> Dir
makeDir dir cs = snd $ makeDir' dir cs
  where
    makeDir' :: Dir -> [Cmd] -> ([Cmd], Dir)
    makeDir' dir (c:cs) = case c of
      CmdLs   -> makeDir' dir cs
      CmdCdUp -> (cs, dir)
      CmdCd s -> let (ncs, nd) = makeDir' (newDir s) cs in
        makeDir' (addDir nd dir) ncs
      ODir _  -> makeDir' dir cs -- ignore in this impl
      OFile _ n -> makeDir' (resizeDir n dir) cs
    makeDir' dir _ = ([], dir)

parseLine :: P.Parsec String () Cmd
parseLine = pCmd P.<|> pDir P.<|> pFile
  where
    pCmd = P.string "$ " >> (pCmdLs P.<|> pCmdCd)
    pCmdLs = P.string "ls" >> return CmdLs
    pCmdCd = P.string "cd " >> (pCmdCdUp P.<|> pCmdCdDown)
    pCmdCdUp = P.string ".." >> return CmdCdUp
    pCmdCdDown = CmdCd <$> P.many P.anyChar
    pDir = P.string "dir " *> (ODir <$> P.many P.anyChar)
    pFile = flip OFile . read <$> P.many P.digit <*> (P.space *> P.many P.anyChar)
