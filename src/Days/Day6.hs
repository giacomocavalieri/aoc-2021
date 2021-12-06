module Days.Day6 ( day6 ) where

import AOC.Day         ( Day(..) )
import Data.List.Split ( splitOn )
import Data.MultiSet   ( MultiSet )
import qualified Data.MultiSet as M ( concatMap, fromList, size )

type TimeToSpawn = Int
type Input       = MultiSet TimeToSpawn
type Output      = Int

parse :: String -> Input
parse = M.fromList . map read . splitOn ","
 
update :: Input -> Input
update = M.concatMap (\i -> if i == 0 then [6, 8] else [i-1])

nthGeneration :: Int -> Input -> Output
nthGeneration n = M.size . (!!n) . iterate update 

partA :: Input -> Output
partA = nthGeneration 80

partB :: Input -> Output
partB = nthGeneration 256

day6 :: Day
day6 = Day 6 parse partA partB