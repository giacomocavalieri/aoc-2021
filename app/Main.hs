module Main where

import AOC.Day            ( Day(..), runDay )
import Days.Day1          ( day1 )
import Days.Day2          ( day2 )
import Days.Day3          ( day3 )
import Days.Day4          ( day4 )
import Days.Day5          ( day5 )
import Days.Day6          ( day6 )
import Days.Day7          ( day7 )
import Days.Day8          ( day8 )
import Days.Day9          ( day9 )
import Days.Day10         ( day10 )
import Days.Day11         ( day11 )
import Days.Day12         ( day12 )
import Days.Day13         ( day13 )
import Days.Day14         ( day14 )
import Days.Day15         ( day15 )
import Days.Day16         ( day16 )
import Days.Day17         ( day17 )
import Days.Day18         ( day18 )
import Days.Day19         ( day19 )
import Days.Day20         ( day20 )
import Days.Day21         ( day21 )
import Days.Day22         ( day22 )
import Days.Day23         ( day23 )
import Days.Day24         ( day24 )
import Days.Day25         ( day25 )
import AOC.Template       ( Mode(..), createDayFile )
import Data.Time.Calendar ( toGregorian )
import Data.Time.Clock    ( getCurrentTime, utctDay )
import System.Environment ( getArgs )

days :: [Day]
days = [ day1 , day2 , day3 , day4 , day5
       , day6 , day7 , day8 , day9 , day10
       , day11, day12, day13, day14, day15
       , day16, day17, day18, day19, day20
       , day21, day22, day23, day24, day25
       ]

data RunMode = All | Last deriving (Eq)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["runAll"]       -> run All
        ["runLast"]      -> run Last
        ["setupAll"]     -> generate Overwrite
        ["setupMissing"] -> generate Preserve
        _                -> error "Unexpected command line argument"

run :: RunMode -> IO ()
run mode = do
    currentDay <- getCurrentDay
    let runnableDays = take currentDay days
        daysToRun    = case mode of
                Last -> [last runnableDays]
                All  -> runnableDays
    mapM_ runDay daysToRun

getCurrentDay :: IO Int
getCurrentDay = do
    now <- getCurrentTime
    let (_, _, day) = toGregorian $ utctDay now
    pure day

generate :: Mode -> IO ()
generate mode = mapM_ (createDayFile mode) [1..25]
