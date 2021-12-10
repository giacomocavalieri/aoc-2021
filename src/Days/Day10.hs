module Days.Day10 ( day10 ) where

import AOC.Day     ( Day(..) )
import Data.Either ( lefts, rights )
import Data.List   ( sort )
import Data.Map    ( (!), fromList )

type UnexpectedChar = Char
type MissingString  = String
type Input          = [String]
type Output         = Int

lookupValue :: [a] -> Char -> a
lookupValue = (!) . fromList . zip ")]}>([{<"

checkLine :: String -> Either UnexpectedChar MissingString
checkLine = checkLine' []
    where checkLine' stack (c:cs)
            | c `elem` ['(', '[', '{', '<']   = checkLine' (c:stack) cs
            | (s:ss) <- stack, s == flipped c = checkLine' ss cs
            | otherwise                       = Left c
          checkLine' stack [] = Right $ map flipped stack
          flipped = lookupValue "([{<)]}>"

partA :: Input -> Output
partA = sum . map (lookupValue [3, 57, 1197, 25137]) . lefts . map checkLine

partB :: Input -> Output
partB = median . map stringValue . rights . map checkLine
    where median xs   = sort xs !! (length xs `div` 2)  
          stringValue = foldl (\acc c -> (acc * 5) + (lookupValue [1..] c)) 0

day10 :: Day
day10 = Day 10 lines partA partB