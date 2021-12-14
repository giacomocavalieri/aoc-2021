module Days.Day14 ( day14 ) where

import AOC.Day         ( Day(..) )
import Data.List.Split ( splitOn )
import Data.Map        ( Map, (!), elems, fromList, fromListWith, toList )
import qualified Data.Map as M ( map )

type Monomer = (Char, Char)
type Rules   = Map Monomer Char
type Polymer = Map Monomer Int
type Input   = (Polymer, Rules)
type Output  = Int

parse :: String -> Input
parse input = (parsePolymer polymer, parseRules rules)
    where [[polymer], rules] = splitOn [""] $ lines input
          parseRules         = fromList . map parseRule
          parseRule r        = let [[c1,c2], [r2]] = splitOn " -> " r in ((c1, c2), r2)
          parsePolymer p     = fromListWith (+) [(monomer, 1) | monomer <- zip p $ tail p]

concatMapWith :: (Ord k, Ord k') => (v -> v -> v) -> ((k', v') -> [(k, v)]) -> Map k' v' -> Map k v
concatMapWith mergeStrategy mapper = fromListWith mergeStrategy . concatMap mapper . toList

growPolymerFor :: Int -> Input -> Polymer
growPolymerFor n (polymer, rules) = (!! n) $ iterate (growPolymer rules) polymer

growPolymer :: Rules -> Polymer -> Polymer
growPolymer rules = concatMapWith (+) newMonomers
    where newMonomers (k@(c1, c2), n) = let c = rules ! k in [((c1, c), n), ((c, c2), n)]

occurrences :: Polymer -> Map Char Int
occurrences = M.map ((`div` 2) . (+1)) . concatMapWith (+) splitMonomer
    where splitMonomer ((c1, c2), n) = [(c1, n), (c2, n)]

solve :: Int -> Input -> Output
solve n = maxMinDiff . elems . occurrences . growPolymerFor n
    where maxMinDiff l = maximum l - minimum l

partA :: Input -> Output
partA = solve 10

partB :: Input -> Output
partB = solve 40

day14 :: Day
day14 = Day 14 parse partA partB