module Days.Day9 ( day9 ) where

import AOC.Day     ( Day(..) )
import Data.Char   ( digitToInt )
import Data.List   ( group, nub, sort )
import Data.Matrix ( Matrix, (!), fromLists, mapPos, safeGet, toList )
import Data.Maybe  ( catMaybes )

type Point       = (Int, Int)
type LowestPoint = Point
type BasinSize   = Int
type Height      = Int
type Input       = Matrix Height
type Output      = Int

parse :: String -> Input
parse = fromLists . map (map digitToInt) . lines

neighbours :: Matrix Height -> Point -> [(Point, Height)]
neighbours m (x, y) = [(p, n) | (p, Just n) <- ns]
    where points = [(x-dx, y-dy) | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1)]]
          ns = map (\(x, y) -> ((x, y), safeGet x y m)) points

flowsTo :: Matrix Height -> Point -> Maybe LowestPoint
flowsTo m p
    | value == 9          = Nothing
    | []   <- reachedLows = Just p
    | [p'] <- reachedLows = Just p'
    | otherwise           = Nothing
    where value           = m ! p
          lowerNeighbours = [p' | (p', value') <- neighbours m p, value' < value]
          reachedLows     = nub $ catMaybes $ map (flowsTo m) lowerNeighbours

basins :: Matrix Height -> [(LowestPoint, BasinSize)]
basins = histogram . catMaybes . toList . mapToReachedLow
    where mapToReachedLow m = mapPos (\p _ -> flowsTo m p) m
          histogram = map (\g -> (head g, length g)) . group . sort

partA :: Input -> Output
partA = sum . map (+1) . getMinimums
    where getMinimums m = map ((m !) . fst) (basins m)

partB :: Input -> Output
partB = product . take 3 . reverse . sort . map snd . basins

day9 :: Day
day9 = Day 9 parse partA partB