module Days.Day25 ( day25 ) where

import Prelude hiding ( Right )
import AOC.Day  ( Day(..) )
import Data.Set ( Set, member, fromList )
import qualified Data.Set as S ( map )

data Direction = Down | Right
type Point     = (Int, Int)
type Herd      = Set Point
data SeaFloor  = SeaFloor Int Int Herd Herd deriving (Eq)
type Input     = SeaFloor
type Output    = Int

parse :: String -> Input
parse input = SeaFloor nRows nCols right down
    where rows  = lines input
          nRows = length rows
          nCols = length $ head $ rows
          right = fromList [ (x+1, y+1) | x <- [0..nRows-1], y <- [0..nCols-1], rows !! x !! y == '>' ]
          down  = fromList [ (x+1, y+1) | x <- [0..nRows-1], y <- [0..nCols-1], rows !! x !! y == 'v' ]

converge :: Eq a => (a -> a) -> a -> [a]
converge f a = map fst $ takeWhile (uncurry (/=)) $ zip iterations $ tail iterations
    where iterations = iterate f a

moveHerds :: SeaFloor -> SeaFloor
moveHerds (SeaFloor maxRow maxCol right down) = SeaFloor maxRow maxCol right' down'
    where right' = S.map (nextPos (SeaFloor maxRow maxCol right down) Right) right
          down'  = S.map (nextPos (SeaFloor maxRow maxCol right' down) Down)  down

nextPos :: SeaFloor -> Direction -> Point -> Point
nextPos (SeaFloor maxRow maxCol right down) dir p
    | p' `member` right || p' `member` down = p
    | otherwise = p'
    where p' = movePoint maxRow maxCol dir p
    
movePoint :: Int -> Int -> Direction -> Point -> Point
movePoint xMax yMax Down  (x, y) = (if x == xMax then 1 else x+1, y)
movePoint xMax yMax Right (x, y) = (x, if y == yMax then 1 else y+1)

partA :: Input -> Output
partA = (+1) . length . converge moveHerds

partB :: Input -> String
partB = const "Merry Christmas!"

day25 :: Day
day25 = Day 25 parse partA partB