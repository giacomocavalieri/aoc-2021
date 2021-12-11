module AOC.Day where

import AOC.Config       ( inputFileName )
import AOC.Download     ( downloadInput )
import System.Directory ( doesFileExist )
import Text.Printf      ( printf )

data Day where
    Day :: (Show a, Show b)
        => Int               -- Day number
        -> (String -> input) -- Input parser
        -> (input -> a)      -- Solution to part A
        -> (input -> b)      -- Solution to part B
        -> Day

runDay :: Day -> IO ()
runDay (Day number parse partA partB) = do
    inputString <- getInput number
    let input = parse inputString
        resA  = show $ partA input
        resB  = show $ partB input
    printf "%da: %s  \t" number resA
    printf "%db: %s\n" number resB

getInput :: Int -> IO String
getInput number = do
    let inputFile = inputFileName number
    exists <- doesFileExist inputFile
    if exists
        then readFile inputFile
        else downloadInput number
