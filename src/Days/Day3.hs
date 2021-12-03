module Days.Day3 ( day3 ) where

import AOC.Day        ( Day(..) )
import Control.Monad  ( join )
import Data.Bifunctor ( bimap )
import Data.Ord       ( comparing )
import Data.List      ( group, maximumBy, partition, sort, transpose )

type Binary = [Int]
type Input  = [Binary]
type Output = Int

parse :: String -> Input
parse = map lineToBinary . lines
    where lineToBinary = map (read . pure)

toInt :: Binary -> Int 
toInt = foldl (\acc bit -> acc*2 + bit) 0

mostCommon :: Binary -> Int
mostCommon bs = if ones >= zeros then 1 else 0
    where (ones, zeros)  = join bimap length $ partition (== 1) bs

sieve :: Int -> (Int -> Int -> Bool) -> [Binary] -> Binary
sieve _ _ [b] = b
sieve pos predicate bs = sieve (pos + 1) predicate $ filter ((predicate common) . (!! pos)) bs
    where common = mostCommon $ map (!! pos) bs

partA :: Input -> Output
partA bs = toInt gammaRate * toInt epsilonRate
    where gammaRate   = map mostCommon $ transpose bs
          epsilonRate = map (1 -) gammaRate

partB :: Input -> Output
partB bs = toInt o2 * toInt co2
    where o2  = sieve 0 (==) bs
          co2 = sieve 0 (/=) bs

day3 :: Day
day3 = Day 3 parse partA partB