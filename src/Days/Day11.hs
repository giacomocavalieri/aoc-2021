module Days.Day11 ( day11 ) where

import AOC.Day             ( Day(..) )
import Control.Monad       ( replicateM )
import Control.Monad.State ( State, evalState, gets, modify )
import Data.Char           ( digitToInt )
import Data.List           ( findIndex )
import Data.Matrix         ( Matrix
                           , (!)
                           , fromLists
                           , getElem
                           , mapPos
                           , ncols
                           , nrows
                           , setElem
                           , toList
                           )
import Data.Maybe          ( fromJust )


data Octopus = Energy Int | Flashed deriving (Show)
type Point   = (Int, Int)
type Flashes = Int 
type Input   = Matrix Octopus
type Output  = Int

parse :: String -> Input
parse = fromLists . map (map (Energy . digitToInt)) . lines

{- Octopi operations -}
inc :: Octopus -> Octopus
inc (Energy n) = Energy $ n + 1
inc Flashed    = Flashed

unflash :: Octopus -> Octopus
unflash (Energy n) = Energy n
unflash Flashed    = Energy 0

withEnergy :: (Int -> Bool) -> Octopus -> Bool
withEnergy pred (Energy n) = pred n
withEnergy _    _          = False

{- Matrix-related utility functions -}
findPoints :: (a -> Bool) -> Matrix a -> [Point]
findPoints pred m = [(x, y) | x <- [1..nrows m], y <- [1..ncols m], pred $ getElem x y m]

updatePoint :: (a -> a) -> Point -> Matrix a -> Matrix a
updatePoint f p@(x, y) m = if inBound then setElem (f $ m ! p) p m else m
    where inBound = 1 <= x && x <= nrows m
                 && 1 <= y && y <= ncols m

updatePoints :: (a -> a) -> [Point] -> Matrix a -> Matrix a
updatePoints f ps m = foldl (flip $ updatePoint f) m ps

updateAll :: (a -> b) -> Matrix a -> Matrix b
updateAll f = mapPos (const f)

neighbours :: Point -> [Point]
neighbours (x, y) = [(x+dx, y+dy) | dx <- [-1..1], dy <- [-1..1], dx /= 0 || dy /= 0]

{- Update logic -}
step :: State (Matrix Octopus) Flashes  -- AOC description of the problem:
step = do modify $ updateAll inc        -- 1. increase energy
          nFlashes <- flash             -- 2. flash all octopi 
          modify $ updateAll unflash    -- 3. reset flashed octopi
          pure nFlashes

flash :: State (Matrix Octopus) Flashes
flash = do
    flashesPos <- gets $ findPoints $ withEnergy (>=10)             -- 1. get the position of all octopi that have to flash
    let nFlashes = length flashesPos
    if nFlashes == 0 then pure 0 else do
        modify $ updatePoints (const Flashed) flashesPos            -- 2. set all octopi with energy >= 10 to Flashed
        let neighbouringPoints = (concatMap neighbours flashesPos)
        modify $ updatePoints inc neighbouringPoints                -- 3. increase the energy of neighbouring octopi
        nFlashes' <- flash                                          -- 4. recursivley try to flash all octopi
        pure $ nFlashes + nFlashes'

partA :: Input -> Output
partA = sum . evalState (replicateM 100 step)

partB :: Input -> Output
partB = (+ 1) . fromJust . findIndex (== 100) . evalState (replicateM 300 step)

day11 :: Day
day11 = Day 11 parse partA partB