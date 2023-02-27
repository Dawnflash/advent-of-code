module Main where

import S1
import S10
import S11
import S12
import S13
import S14
import S15
import S16
import S17
import S18
import S19
import S2
import S20
import S21
import S22
import S23
import S24
import S25
import S3
import S4
import S5
import S6
import S7
import S8
import S9
import System.Environment (getArgs)

main :: IO ()
main = do
  argStage : args <- getArgs
  let stage = read argStage :: Int
  let prefix = case args of
        (x : _) -> x
        _ -> ""
  input <- readFile $ "../inputs/" ++ prefix ++ show stage
  case stage of
    1 -> S1.main input
    2 -> S2.main input
    3 -> S3.main input
    4 -> S4.main input
    5 -> S5.main input
    6 -> S6.main input
    7 -> S7.main input
    8 -> S8.main input
    9 -> S9.main input
    10 -> S10.main input
    11 -> S11.main input
    12 -> S12.main input
    13 -> S13.main input
    14 -> S14.main input
    15 -> S15.main input
    16 -> S16.main input
    17 -> S17.main input
    18 -> S18.main input
    19 -> S19.main input
    20 -> S20.main input
    21 -> S21.main input
    22 -> S22.main input
    23 -> S23.main input
    24 -> S24.main input
    25 -> S25.main input
    _ -> print "unknown stage"
