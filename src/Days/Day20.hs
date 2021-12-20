module Days.Day20 ( day20 ) where

import Prelude hiding ( Left, Right, repeat )
import AOC.Day ( Day(..) )
import Data.List.Split ( splitOn )
import Data.Matrix ( Matrix, (<->), (<|>), fromLists, mapPos, matrix, ncols, nrows, safeGet, toList )

type Bit       = Int
type Image     = Matrix Bit
type Algorithm = [Bit]
type Input     = (Algorithm, Matrix Bit)
type Output    = Int

parse :: String -> Input
parse = parseInput . splitOn "\n\n"
    where parseInput [alg, img] = (map charToBin alg, parseImg img)
          parseImg = fromLists . map (map charToBin) . lines
          charToBin '.' = 0
          charToBin '#' = 1

toInt :: [Bit] -> Int
toInt = foldl (\acc b -> acc * 2 + b) 0

pad :: a -> Matrix a -> Matrix a
pad defaultValue m = row <-> (col <|> m <|> col) <-> row
    where (rows, cols)  = (nrows m, ncols m)
          col = matrix rows 1 (const defaultValue)
          row = matrix 1 (cols+2) (const defaultValue)

getOrElse :: a -> Int -> Int -> Matrix a -> a
getOrElse defaultValue x y m = case safeGet x y m of
    Nothing -> defaultValue
    Just a  -> a

safeNeighbours :: a -> Int -> Int -> Matrix a -> [a]
safeNeighbours border x y m = [getOrElse border (x+dx) (y+dy) m | dx <- [-1..1], dy <- [-1..1]]

enhance :: Bit -> Algorithm -> Image -> Image
enhance border algorithm m = mapPos (const . updatePoint) m'
    where updatePoint (x, y) = algorithm !! toInt (safeNeighbours border x y m')
          m' = pad border m

enhanceMany :: Int -> Algorithm -> Image -> Image
enhanceMany times algorithm m = snd $ (iterate f (0, m)) !! times
    where f (border, m) = (newBorder border, enhance border algorithm m)
          newBorder 0 = head algorithm
          newBorder 1 = last algorithm

solve :: Int -> (Algorithm, Image) -> Int
solve times = length . filter (== 1) . toList . uncurry (enhanceMany times)

partA :: Input -> Output
partA = solve 2

partB :: Input -> Output
partB = solve 50

day20 :: Day
day20 = Day 20 parse partA partB