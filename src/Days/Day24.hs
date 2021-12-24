module Days.Day24 ( day24 ) where

import AOC.Day         ( Day(..) )
import Data.List       ( sort, transpose )
import Data.List.Split ( chunksOf )
import Data.Map        ( Map, empty, insert, toList )

data Placeholder = Placeholder Int        deriving (Eq, Ord, Show)
data Offset      = Offset Placeholder Int deriving (Eq, Ord, Show)
data Constraint  = Constraint Placeholder Offset deriving (Show)
type Input       = [Int]
type Output      = Int

{- 
   MONAD working logic
   z is a stack with numbers in range [1, 26], for each digit of the input
   
       z@(d:ds) addx addy
       | addx > 0      = (w + addy):z
       | d + addx /= w = (w + addy):ds
       | d + addx == w = ds
   
   at the end it is successful if z is empty, the only way for z to be empty
   is for half of the addx to be positive and the other half to be negative.
   For the negative addx it is necessary that `d + addx /= w` so that an element
   is removed from the stack each time.

   addx and addy are the only variable parts in the MONAD code: they are the literal
   values respectively at line 5 and 15 of each block of 18 lines (they always
   the same except for those 2 variables).
-}

parse :: String -> Input
parse = combine . transpose . map (extractValue . getVariableLines) . splitCycles
    where splitCycles        = chunksOf 18 . lines
          getVariableLines l = [l !! 5, l !! 15] 
          extractValue       = map (read . drop 6)
          combine [xs, ws]   = zipWith keepOne xs ws
          keepOne x w        = if x < 0 then x else w

findConstraints :: Input -> [Constraint]
findConstraints = snd . foldl extractConstraint ([], []) . zip (map Placeholder [1..])
    where extractConstraint (stack, cs) (placeholder, n)
            | n >= 0    = (Offset placeholder n : stack, cs)
            | otherwise = (tail stack, Constraint placeholder (Offset p (n + m)) : cs)
            where (Offset p m) = head stack

solveConstraints :: (Int -> (Int, Int)) -> [Constraint] -> Map Placeholder Int
solveConstraints _ [] = empty
solveConstraints f ((Constraint d1 (Offset d2 o)):cs) = m'
    where (v1, v2) = f o
          m  = solveConstraints f cs 
          m' = insert d1 v1 $ insert d2 v2 m 

toNumber :: Map Placeholder Int -> Int
toNumber = foldl (\acc (_, d) -> acc*10 + d) 0 . sort . toList 

partA :: Input -> Output
partA = toNumber . solveConstraints solver . findConstraints
    where solver o = if o >= 0 then (9, 9 - o) else (9 + o, 9)

partB :: Input -> Output
partB = toNumber . solveConstraints solver . findConstraints
    where solver o = if o >= 0 then (o + 1, 1) else (1, 1 - o)

day24 :: Day
day24 = Day 24 parse partA partB