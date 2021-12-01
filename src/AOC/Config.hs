module AOC.Config ( codeFileName, inputFileName, getCookie, getYear ) where

import Network.HTTP.Client ( Cookie )
import System.Directory    ( getCurrentDirectory )
import System.FilePath     ( takeBaseName )

codeFileName :: Int -> String
codeFileName number = "./src/Days/Day" ++ show number ++ ".hs"

inputFileName :: Int -> String
inputFileName number = "./inputs/" ++ show number ++ ".txt"

getYear :: IO Int
getYear = do
    dir <- getCurrentDirectory
    -- expectes the working directory to be "aoc-yyyy"
    pure $ read $ drop 4 $ takeBaseName dir

getCookie :: IO Cookie
getCookie = read <$> readFile "./conf/cookie"
