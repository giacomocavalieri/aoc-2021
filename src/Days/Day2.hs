module Days.Day2 ( day2 ) where

import AOC.Day   ( Day(..) )
import Data.Char ( toUpper )

data Command = Up Int | Down Int | Forward Int deriving (Show, Read)
type Coordinates = (Int, Int, Int)
type Input = [Command]
type Output = Int

parse :: String -> Input
parse = map (read . capitalized) . lines
    where capitalized s = toUpper (head s) : tail s

update :: Coordinates -> Command -> Coordinates
update (x, y, aim) (Up n)      = (x, (y - n), aim)
update (x, y, aim) (Down n)    = (x, (y + n), aim)
update (x, y, aim) (Forward n) = ((x + n), y, aim)

updateAim :: Coordinates -> Command -> Coordinates
updateAim (x, y, aim) (Up n)      = (x, y, (aim - n))
updateAim (x, y, aim) (Down n)    = (x, y, (aim + n))
updateAim (x, y, aim) (Forward n) = ((x + n), (y + aim*n), aim)

getResult :: Coordinates -> Output
getResult (x, y, _) = x * y

partA :: Input -> Output
partA = getResult . foldl update (0, 0, 0)

partB :: Input -> Output
partB = getResult . foldl updateAim (0, 0, 0)

day2 :: Day
day2 = Day 2 parse partA partB