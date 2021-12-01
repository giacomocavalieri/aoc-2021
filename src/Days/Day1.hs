module Days.Day1 ( day1 ) where

import AOC.Day ( Day(..) )

type Input = [Int]

parse :: String -> Input
parse = map read . lines

partA :: Input -> Int
partA = countIncreases

partB :: Input -> Int
partB xs = countIncreases xs'
    where xs' = zipWith3 (\a b c -> a + b + c) xs (tail xs) (drop 2 xs)

countIncreases :: [Int] -> Int
countIncreases xs = length $ filter id $ zipWith (<) xs (tail xs)

day1 :: Day
day1 = Day 1 parse partA partB
