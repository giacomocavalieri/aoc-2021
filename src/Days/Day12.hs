module Days.Day12 ( day12 ) where

import AOC.Day         ( Day(..) )
import Data.Char       ( isLower )
import Data.List.Split ( splitOn )
import Data.Map        ( Map
                       , (!)
                       , empty
                       , filterWithKey
                       , findWithDefault
                       , fromListWith
                       , insertWith
                       , notMember
                       )
import qualified Data.Map as M ( null )

data Cave = Start | End | Small String | Big String deriving ( Eq, Ord, Show )
type Graph a = Map a [a]
type Path a = [a]
type Input = Graph Cave
type Output = Int

parse :: String -> Input
parse = fromListWith (++) . concatMap parseConnection . lines
    where parseConnection = toTuple . map parseCave . splitOn "-"
          toTuple [x, y] = [(x, [y]), (y, [x])]

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end"   = End
parseCave s@(c:cs)
    | isLower c = Small s
    | otherwise = Big s

countPaths :: Ord a => (Map a Int -> a -> Bool) -> Map a Int -> a -> a -> Graph a -> Int
countPaths canVisit visited from to graph = allPaths + if to `elem` reachedNodes then 1 else 0
    where reachedNodes = graph ! from
          visited'     = insertWith (+) from 1 visited
          pathsFrom n  = countPaths canVisit visited' n to graph
          allPaths     = sum $ map pathsFrom $ filter (canVisit visited') reachedNodes

canVisit :: (Map Cave Int -> Cave -> Bool) -> Map Cave Int -> Cave -> Bool
canVisit _    _       (Big _) = True
canVisit _    _       Start   = False
canVisit _    _       End     = False
canVisit pred visited small   = pred visited small

partA :: Input -> Output
partA = countPaths pred empty Start End
    where pred = canVisit $ \visited small -> findWithDefault 0 small visited == 0

partB :: Input -> Output
partB = countPaths pred empty Start End
    where isSmall (Small _) = True
          isSmall _         = False
          pred = canVisit $ \visited small -> small `notMember` visited
                                           || M.null (filterWithKey (\k v -> isSmall k && v > 1) visited)

day12 :: Day
day12 = Day 12 parse partA partB