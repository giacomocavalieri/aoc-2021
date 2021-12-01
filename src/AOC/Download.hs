module AOC.Download ( downloadInput ) where

import AOC.Config            ( inputFileName, getCookie, getYear )
import Data.ByteString.Char8 ( unpack )
import Network.HTTP.Client   ( createCookieJar )
import Network.HTTP.Req      ( (/~)
                             , (/:)
                             , GET(..)
                             , NoReqBody(..)
                             , bsResponse
                             , cookieJar
                             , defaultHttpConfig
                             , https
                             , req
                             , responseBody
                             , runReq
                             )
import Text.Printf           ( printf )

downloadInput :: Int -> IO String
downloadInput number = do
    printf "Downloading input for day %d\n" number
    downloadedInput <- makeRequest number
    writeFile (inputFileName number) downloadedInput
    pure downloadedInput

makeRequest :: Int -> IO String
makeRequest number = do
    cookie <- getCookie
    year   <- getYear
    let url     = https "adventofcode.com" /~ year /: "day" /~ number /: "input"
        cookies = cookieJar $ createCookieJar [cookie]
        request = req GET url NoReqBody bsResponse cookies
    response <- runReq defaultHttpConfig request
    pure $ unpack $ responseBody $ response
