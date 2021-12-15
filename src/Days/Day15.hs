module Days.Day15 ( day15 ) where

import AOC.Day                        ( Day(..) )
import Data.Char                      ( digitToInt )
import Data.Matrix                    ( Matrix, (!), (<|>), (<->), fromLists, mapPos, safeGet, toList )
import Data.Maybe                     ( fromJust )
import Graph.DijkstraSimple           ( Graph(..), EdgeTo(..), findPath, pathWeight )
import Graph.DijkstraSimple.Weighters ( cumulativeWeighter )
import qualified Data.Map as Map      ( fromList, toList )
import qualified Data.Matrix as Mat   ( fromLists, toList )

type Point  = (Int, Int)
type Input  = Matrix Int
type Output = Int

parse :: String -> Input
parse = fromLists . map (map digitToInt) . lines

matrixToGraph :: Matrix Int -> Graph Point Int
matrixToGraph = Graph . Map.fromList . Mat.toList . arcs
    where arcs m = mapPos (const . pointToArcs m) m 

pointToArcs :: Matrix a -> Point -> (Point, [EdgeTo Point a])
pointToArcs m p@(x, y) = (p, edges)
    where allPoints   = [(x-dx, y-dy) | (dx, dy) <-[(-1, 0), (0, -1), (1, 0), (0, 1)]]
          validPoints = [(a, p) | p@(x, y) <- allPoints, (Just a) <- [safeGet x y m]]
          edges       = [EdgeTo p a | (a, p) <- validPoints]

increaseRisk :: Int -> Matrix Int -> Matrix Int
increaseRisk n m = foldl1 (<->) $ take n $ iterate incMat row
    where incMat = fmap (\x -> if x == 9 then 1 else x+1)
          row    = foldl1 (<|>) $ take n $ iterate incMat m

shortestPath :: Graph Point Int -> Point -> Point -> Int
shortestPath g start end = pathWeight $ fromJust $ findPath g start cumulativeWeighter end 

partA :: Input -> Output
partA m = shortestPath (matrixToGraph m) (1, 1) (100, 100)

partB :: Input -> Output
partB m = shortestPath (matrixToGraph $ increaseRisk 5 m) (1, 1) (500, 500)

day15 :: Day
day15 = Day 15 parse partA partB