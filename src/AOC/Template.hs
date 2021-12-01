module AOC.Template ( Mode(..), createDayFile ) where

import AOC.Config        ( codeFileName )
import Data.Text         ( pack, unpack )
import NeatInterpolation ( trimming )
import System.Directory  ( doesFileExist )
import Text.Printf       ( printf )

data Mode = Overwrite | Preserve deriving (Eq)

createDayFile :: Mode -> Int -> IO ()
createDayFile mode number = do
    let fileName = codeFileName number
    exists <- doesFileExist fileName
    if mode == Overwrite || not exists 
        then printf "Creating file %s\n" fileName
          >> writeFile fileName (dayFileTemplate number)
        else printf "File %s already exists\n" fileName

dayFileTemplate :: Int -> String
dayFileTemplate day = unpack [trimming|
    module Days.Day$dayNumber ( $dayString ) where

    import AOC.Day ( Day(..) )

    type Input = [Int]
    type Output = String

    parse :: String -> Input
    parse = undefined

    partA :: Input -> Output
    partA = const "TODO"

    partB :: Input -> Output
    partB = const "TODO"

    $dayString :: Day
    $dayString = Day $dayNumber parse partA partB

    |]
    where dayNumber = pack $ show day
          dayString = pack $ "day" ++ show day