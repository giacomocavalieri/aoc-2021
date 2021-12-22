module Days.Day22 ( day22 ) where

import AOC.Day         ( Day(..) )
import Data.List.Split ( splitOn )
import Data.Maybe      ( catMaybes )
import Data.MultiSet   ( MultiSet, empty, insert, mapMaybe, union )
import qualified Data.MultiSet as MS ( map )

data Range       = Range Int Int deriving (Eq, Ord, Show)
data Cuboid      = Cuboid Range Range Range deriving (Eq, Ord, Show)
type Instruction = (String, Cuboid)  
type State       = (MultiSet Cuboid, MultiSet Cuboid)
type Input       = [Instruction]
type Output      = Int

class Intersectable a where
    cardinality :: a -> Int
    (∩) :: a -> a -> Maybe a

instance Intersectable Range where
    cardinality (Range a b) = b - a + 1
    (Range a b) ∩ (Range c d)
        | x <= y    = Just $ Range x y
        | otherwise = Nothing 
        where x = max a c
              y = min b d

instance Intersectable Cuboid where
    cardinality (Cuboid xr yr zr) = product $ map cardinality [xr, yr, zr]
    (Cuboid xr yr zr) ∩ (Cuboid xr' yr' zr') = do
        xr'' <- xr ∩ xr'
        yr'' <- yr ∩ yr'
        zr'' <- zr ∩ zr'
        pure $ Cuboid xr'' yr'' zr''

parse :: String -> Input
parse = map (parseLine . splitOn " ") . lines
    where parseLine [op, coords] = (op, toCuboid . map parseCoordinate $ splitOn "," coords)
          parseCoordinate = toRange . map read . splitOn ".." . drop 2 
          toCuboid [a, b, c] = Cuboid a b c
          toRange [a, b] = Range a b

applyInstruction :: State -> Instruction -> State
applyInstruction (on, off) (op, cuboid)
    | op == "on"  = (insert cuboid on', off')
    | op == "off" = (on', off')
    where toTurnOn  = mapMaybe (∩ cuboid) off
          toTurnOff = mapMaybe (∩ cuboid) on
          on'  = union on toTurnOn
          off' = union off toTurnOff

points :: State -> Int
points (on, off) = totalPoints on - totalPoints off
    where totalPoints = sum . MS.map cardinality

solve :: [Instruction] -> Int
solve = points . foldl applyInstruction (empty, empty)

partA :: Input -> Output
partA = solve . take 20

partB :: Input -> Output
partB = solve

day22 :: Day
day22 = Day 22 parse partA partB