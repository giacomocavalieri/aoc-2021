module Days.Day21 ( day21 ) where

import Prelude             hiding ( lookup )
import AOC.Day             ( Day(..) )
import Control.Monad       ( replicateM )
import Control.Monad.State ( State, evalState, modify, get, gets, runState )
import Data.Char           ( digitToInt )
import Data.Map            ( Map, empty, insert, lookup )
import Data.Maybe          ( fromMaybe )
import Data.Tuple          ( swap )

type Input  = (Position, Position)
type Output = Int

type Position   = Int
type Roll       = Int
type Dice       = Int
type Score      = Int
type Results    = (Int, Int)
data Game       = Game (Position, Score) (Position, Score) deriving (Eq, Ord, Show)
type SavedGames = Map Game Results

parse :: String -> Input
parse = (\[x, y] -> (x, y)) . map (digitToInt . last) . lines

(<+>) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(x, y) <+> (z, w) = (x+z, y+w)

getOrElse :: Maybe a -> a -> a
getOrElse = flip fromMaybe

playDeterministic :: Game -> State Dice Game
playDeterministic game@(Game (_, s1) (_, s2))
    | s1 >= 1000 || s2 >= 1000 = pure game
    | otherwise = do
        rolls <- sequence [roll, roll, roll]
        playDeterministic $ stepGame game rolls
        where roll = modify (+1) >> get

playDirac :: Game -> State SavedGames Results
playDirac game@(Game (_, s1) (_, s2))
    | s1 >= 21 = pure (1, 0)
    | s2 >= 21 = pure (0, 1)
    | otherwise = do
        savedResult <- gets $ fmap pure . lookup game
        getOrElse savedResult $ do
            let allRolls = replicateM 3 [1..3]
            let allGames = map (stepGame game) allRolls
            allResults <- map swap <$> mapM playDirac allGames
            let result = foldl (<+>) (0, 0) allResults
            modify $ insert game result
            pure result

stepGame :: Game -> [Roll] -> Game
stepGame (Game (p1, s1) (p2, s2)) rolls = Game (p2, s2) (p1', s1')
    where p1' = move p1 rolls
          s1' = s1 + p1'

move :: Position -> [Roll] -> Position
move starting rolls = ((starting + (sum rolls) - 1) `mod` 10) + 1

partA :: Input -> Output
partA (p1, p2) = rollsCount * min s1 s2
    where startingGame = Game (p1, 0) (p2, 0)
          result = runState (playDeterministic startingGame) 0
          (Game (_, s1) (_, s2), rollsCount) = result

partB :: Input -> Output
partB (p1, p2) = max s1 s2
    where startingGame = Game (p1, 0) (p2, 0)
          (s1, s2) = evalState (playDirac startingGame) empty

day21 :: Day
day21 = Day 21 parse partA partB