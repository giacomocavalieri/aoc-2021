module Days.Day8 ( day8 ) where

import AOC.Day         ( Day(..) )
import Data.List       ( (\\), partition, intersect, sort )
import Data.List.Split ( splitOn )
import Data.Map        ( Map )
import qualified Data.Map as M ( (!), fromList )

type Input  = [([String], [String])]
type Output = Int

parse :: String -> Input
parse = map parseLine . lines
    where parseLine = toTuple . map (words) . splitOn " | "
          toTuple [x, y] = (x, y)

signalToNumber :: String -> Int
signalToNumber "abcefg"  = 0
signalToNumber "cf"      = 1
signalToNumber "acdeg"   = 2
signalToNumber "acdfg"   = 3
signalToNumber "bcdf"    = 4
signalToNumber "abdfg"   = 5
signalToNumber "abdefg"  = 6
signalToNumber "acf"     = 7
signalToNumber "abcdefg" = 8
signalToNumber "abcdfg"  = 9

findWiring :: [String] -> Map Char Char
findWiring xs = M.fromList [(a, 'a'), (b, 'b'), (c, 'c'), (d, 'd'), (e, 'e'), (f, 'f'), (g, 'g')]
    where [one]        = filter ((== 2) . length) xs
          [four]       = filter ((== 4) . length) xs
          [seven]      = filter ((== 3) . length) xs
          [eight]      = filter ((== 7) . length) xs
          zeroSixNine  = filter ((== 6) . length) xs
          twoThreeFive = filter ((== 5) . length) xs
          (zeroNine, [six])  = partition ((== 2) . length . intersect one) zeroSixNine
          ([nine], [zero])   = partition (elem d) zeroNine 
          ([five], twoThree) = partition (null . (\\ six)) twoThreeFive
          ([two], [three])   = partition ((== 3) . length . intersect five) twoThree

          [a] = seven \\ one
          [b] = five \\ three
          [c] = nine \\ six
          [d] = foldl intersect four twoThreeFive
          [e] = zero \\ nine
          [f] = seven \\ two
          [g] = (nine \\ four) \\ seven

decode :: Map Char Char -> String -> Int
decode wiring = signalToNumber . sort . map (wiring M.!)

solve :: ([String], [String]) -> Int
solve (input, output) = digitsToNumber $ map (decode wiring) output
    where wiring         = findWiring input
          digitsToNumber = read . concatMap show

partA :: Input -> Output
partA input = sum [1 | (_, signals) <- input, s <- signals, isUniqueDigit s]
    where isUniqueDigit s = length s `elem` [2, 4, 3, 7]

partB :: Input -> Output
partB = sum . map solve

day8 :: Day
day8 = Day 8 parse partA partB