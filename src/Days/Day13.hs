module Days.Day13 ( day13 ) where

import AOC.Day         ( Day(..) )
import Data.Bifunctor  ( first, second )
import Data.List.Split ( splitOn )
import Data.Set        ( Set, fromList )
import qualified Data.Set as S ( map, member )

newtype PlainString = PlainString String
instance Show PlainString where
  show (PlainString s) = s

data Instruction = Y Int | X Int
type Point       = (Int, Int)
type Input       = (Set Point, [Instruction])
type OutputA     = Int
type OutputB     = PlainString

parse :: String -> Input
parse input = (points, folds)
    where [ps, is] = splitOn [""] $ lines input
          points   = fromList $ map (read . (\s -> "(" ++ s ++ ")")) ps
          folds    = map (parseFold . drop 11) is
          parseFold ('y':'=':n) = Y $ read n
          parseFold ('x':'=':n) = X $ read n

foldPaper :: Set Point -> Instruction -> Set Point
foldPaper ps instruction
    | (Y n) <- instruction = S.map (second $ flipAlong n) ps
    | (X n) <- instruction = S.map (first  $ flipAlong n) ps
    where flipAlong along m = if along < m then m - 2 * (m - along) else m

graph :: Set Point -> String
graph ps = unlines [[toChar x y | x <- [0..maxX]] | y <- [0..maxY] ]
  where toChar x y = if (x, y) `S.member` ps then '▒' else '.'
        maxY = maximum $ S.map snd ps
        maxX = maximum $ S.map fst ps

partA :: Input -> OutputA
partA (ps, i:_) = length $ foldPaper ps i

partB :: Input -> OutputB
partB = PlainString . ('\n':) . graph . uncurry (foldl foldPaper)

day13 :: Day
day13 = Day 13 parse partA partB