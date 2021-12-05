module Days.Day5 ( day5 ) where

import AOC.Day         ( Day(..) )
import Data.List       ( (\\), group, sort, sortOn )
import Data.List.Split ( splitOn )

type Point   = (Int, Int)
type Segment = (Point, Point)
type Input   = [Segment]
type Output  = Int

parse :: String -> Input
parse = map parseSegment . lines
    where parseSegment   = toTuple . map parsePoint . splitOn " -> "
          parsePoint   p = read $ concat ["(", p, ")"]
          toTuple [x, y] = (x, y)

toPoints :: Segment -> [Point]
toPoints ((x1, y1), (x2, y2))
    | x1 == x2  = [(x1, y) | y <- y1...y2]
    | y1 == y2  = [(x, y1) | x <- x1...x2]
    | otherwise = zip (x1...x2) (y1...y2)

(...) :: Int -> Int -> [Int]
x1 ... x2
    | x1 <= x2  = [x1..x2]
    | otherwise = reverse [x2..x1]

histogram :: Ord a => [a] -> [(a, Int)]
histogram = map (\g -> (head g, length g)) . group . sort

countDangerZones :: Input -> Int
countDangerZones = length . filter ((> 1) . snd) . histogram . concatMap toPoints

partA :: Input -> Output
partA = countDangerZones . filter isPerpendicular
    where isPerpendicular ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

partB :: Input -> Output
partB = countDangerZones

day5 :: Day
day5 = Day 5 parse partA partB