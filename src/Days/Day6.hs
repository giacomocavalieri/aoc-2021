module Days.Day6 ( day6 ) where

import AOC.Day   ( Day(..) )
import Data.List ( group, sort )
import Data.Map  ( Map )
import qualified Data.Map as M ( elems, findWithDefault, fromList, insertWith, mapKeysWith )

type Input = Map Int Int
type Output = Int

parse :: String -> Input
parse s = histogram $ read $ concat ["[", s, "]"]
    where histogram = M.fromList . map (\g -> (head g, length g)) . group . sort 
 
update :: Input -> Input
update xs = M.insertWith (+) 8 zeros decreased 
    where zeros      = M.findWithDefault 0 0 xs
          decrease k = if k > 0 then k - 1 else 6
          decreased  = M.mapKeysWith (+) decrease xs

totalPopulation :: Input -> Int
totalPopulation = sum . M.elems

nthGeneration :: Int -> Input -> Input
nthGeneration n = (!!n) . iterate update 

partA :: Input -> Output
partA = totalPopulation . nthGeneration 80

partB :: Input -> Output
partB = totalPopulation . nthGeneration 256

day6 :: Day
day6 = Day 6 parse partA partB