module Days.Day17 ( day17 ) where

import AOC.Day ( Day(..) )
import Data.List ( sort, sortOn, takeWhile )
import Data.List.Split ( splitOn )
import Data.Maybe ( catMaybes, isJust )

type Point    = (Int, Int)
type Velocity = (Int, Int)
type Range    = (Int, Int)

type Input    = (Range, Range)
type Output   = Int

parse :: String -> Input
parse = toTuple . map parseRange . splitOn ", " . drop 13
    where parseRange = toTuple . sort . map read . splitOn ".." . drop 2
          toTuple [a, b] = (a, b)

contains :: Range -> Int -> Bool
contains (min, max) v = min <= v && v <= max

trajectory :: (Range, Range) -> Point -> Velocity -> Maybe [Point]
trajectory rs@(rx@(minX, maxX), ry@(minY, maxY)) (x, y) (vx, vy)
    | neverReaches = Nothing
    | inRange      = Just [(x, y)]
    | otherwise    = fmap ((x, y) :) $ trajectory rs (x + vx, y + vy) (vx', vy - 1)
    where vx' = if vx > 0 then vx - 1 else 0
          inRange      = rx `contains` x && ry `contains` y
          neverReaches = x > maxX || y < minY

reachesXBound :: Range -> Int -> Int -> Bool
reachesXBound rx x 0 = rx `contains` x
reachesXBound rx x v = rx `contains` x || reachesXBound rx (x+v) (v-1)

candidateVelocities :: (Range, Range) -> Point -> [Velocity]
candidateVelocities (rx@(minX, maxX), (minY, maxY)) (x, y) =
    [(vx, vy) | vy <- [(-200)..162], vx <- [0..76], reachesXBound rx x vx]

partA :: Input -> Output
partA r = last $ sort $ map (maxY . snd) $ paths
    where startingPoint = (0, 0)
          vs    = candidateVelocities r startingPoint
          ts    = map (trajectory r startingPoint) vs
          paths = [(v, t) | (v, Just t) <- vs `zip` ts]
          maxY  = maximum . map snd

partB :: Input -> Output
partB r = length $ paths
    where startingPoint = (0, 0)
          vs    = candidateVelocities r startingPoint
          ts    = map (trajectory r startingPoint) vs
          paths = [(v, t) | (v, Just t) <- vs `zip` ts]

day17 :: Day
day17 = Day 17 parse partA partB