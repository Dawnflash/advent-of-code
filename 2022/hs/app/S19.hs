module S19 where

import qualified Control.Parallel.Strategies as ParS
import Data.Maybe (catMaybes)
import Lib
import qualified Text.Parsec as P

data Blueprint = Blueprint
  { blueprintId :: Int,
    costOre :: Int, -- ore
    costClay :: Int, -- ore
    costObsidian :: (Int, Int), -- ore, clay
    costGeode :: (Int, Int) -- ore, obsidian
  }
  deriving (Show)

data Resources = Resources
  { ore :: Int,
    clay :: Int,
    obsidian :: Int,
    geode :: Int
  }
  deriving (Show)

data Stock = Stock {resources :: Resources, robots :: Resources} deriving (Show)

-- we start with 1 ore robot and no resources
newStock :: Stock
newStock = Stock (Resources 0 0 0 0) (Resources 1 0 0 0)

buyOreRobot :: Blueprint -> Stock -> Maybe Stock
buyOreRobot bp (Stock rs rb)
  | o < c = Nothing
  | otherwise = Just $ Stock rs {ore = o - c} rb {ore = ore rb + 1}
  where
    o = ore rs
    c = costOre bp

buyClayRobot :: Blueprint -> Stock -> Maybe Stock
buyClayRobot bp (Stock rs rb)
  | o < c = Nothing
  | otherwise = Just $ Stock rs {ore = o - c} rb {clay = clay rb + 1}
  where
    o = ore rs
    c = costClay bp

buyObsidianRobot :: Blueprint -> Stock -> Maybe Stock
buyObsidianRobot bp (Stock rs rb)
  | o < co || c < cc = Nothing
  | otherwise = Just $ Stock rs {ore = o - co, clay = c - cc} rb {obsidian = obsidian rb + 1}
  where
    o = ore rs
    c = clay rs
    (co, cc) = costObsidian bp

buyGeodeRobot :: Blueprint -> Stock -> Maybe Stock
buyGeodeRobot bp (Stock rs rb)
  | o < co || b < cb = Nothing
  | otherwise = Just $ Stock rs {ore = o - co, obsidian = b - cb} rb {geode = geode rb + 1}
  where
    o = ore rs
    b = obsidian rs
    (co, cb) = costGeode bp

-- produce a list of feasible purchase options
buyOptions :: Blueprint -> Stock -> [Stock]
buyOptions bp stock
  | (Just nstock) <- buyGeodeRobot bp stock = [nstock] -- always buy a geode robot
  | (Just nstock) <- buyObsidianRobot bp stock = [nstock] -- always buy an obsidian robot
  | otherwise = stock : catMaybes [buyClayRobot bp stock, buyOreRobot bp stock]

addProduce :: Resources -> Stock -> Stock
addProduce prod stock =
  stock
    { resources =
        rs
          { ore = ore rs + ore prod,
            clay = clay rs + clay prod,
            obsidian = obsidian rs + obsidian prod,
            geode = geode rs + geode prod
          }
    }
  where
    rs = resources stock

step :: Blueprint -> Stock -> Stock
step b s = addProduce (robots s) . head $ buyOptions b s

main :: String -> IO ()
main input = do
  let blueprints = parseLines parseLine $ lines input

  print $ sum $ ParS.parMap ParS.rdeepseq (score1 24) blueprints -- part 1 (takes minutes - 20s per thread?)
  print $ product $ ParS.parMap ParS.rdeepseq (score 32) $ take 3 blueprints -- part 2 (takes about an hour)

-- scoring for part 1
score1 :: Int -> Blueprint -> Int
score1 n bp = score n bp * blueprintId bp

-- max geodes
score :: Int -> Blueprint -> Int
score n bp = score' n newStock
  where
    -- most geodes attainable
    score' :: Int -> Stock -> Int
    score' 0 stock = geode (resources stock)
    score' n stock = maximum $ map (score' (n - 1) . addProduce (robots stock)) opts
      where
        -- also includes the "buy nothing scenario"
        -- opts = stock : catMaybes [buyOreRobot bp stock, buyClayRobot bp stock, buyObsidianRobot bp stock, buyGeodeRobot bp stock]
        opts = buyOptions bp stock

parseLine :: ParserT Blueprint
parseLine = do
  P.string "Blueprint "
  i <- parseInt
  P.string ": Each ore robot costs "
  oc <- parseInt
  P.string " ore. Each clay robot costs "
  cc <- parseInt
  P.string " ore. Each obsidian robot costs "
  bco <- parseInt
  P.string " ore and "
  bcc <- parseInt
  P.string " clay. Each geode robot costs "
  gco <- parseInt
  P.string " ore and "
  gcb <- parseInt
  P.string " obsidian."
  return $ Blueprint i oc cc (bco, bcc) (gco, gcb)
