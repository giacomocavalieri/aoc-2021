module Days.Day7 ( day7 ) where

import AOC.Day         ( Day(..) )
import Data.List.Split ( splitOn )

type Position = Int
type Steps    = Int
type Cost     = Int
type Input    = [Position]
type Output   = Cost

parse :: String -> Input
parse = map read . splitOn ","

costs :: (Steps -> Cost) -> Input -> [Cost]
costs costFunction xs = map costToAlignTo allPositions
    where allPositions = [minimum xs..maximum xs]
          costToAlignTo pos = sum $ map (costFunction . abs . (-) pos) xs

partA :: Input -> Output
partA = minimum . costs id

partB :: Input -> Output
partB = minimum . costs sumUpToN
    where sumUpToN n = n * (n+1) `div` 2

day7 :: Day
day7 = Day 7 parse partA partB