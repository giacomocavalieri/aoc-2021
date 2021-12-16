module Days.Day16 ( day16 ) where

import AOC.Day     ( Day(..) )
import Data.Char   ( digitToInt )
import Data.Either ( fromRight )
import Data.List   ( intercalate )
import Data.Map    ( (!), fromList )
import Text.Parsec ( Parsec, count, many1, oneOf )
import qualified Text.Parsec as P ( parse )

type Parser    = Parsec String ()
type Binary    = String
type Version   = Int
data Operation = Sum | Product | Min | Max | Greater | Less | Equal deriving (Enum)
data Packet    = LiteralValue Version Int | Operator Version Operation [Packet]
type Input     = Packet
type Output    = Int

parse :: String -> Input
parse = fromRight undefined . P.parse packet "" . concatMap hexToBinary . init

{- Hexadecimal and binary convertions -}
hexToBinary :: Char -> Binary
hexToBinary = (!) $ fromList $ "0123456789ABCDEF" `zip` binary
    where binary = [[b1,b2,b3,b4] | b1 <- "01", b2 <- "01", b3 <- "01", b4 <- "01"]

binaryToInt :: Binary -> Int
binaryToInt = foldl (\acc c -> acc * 2 + digitToInt c) 0

{- Operation conversions -}
intToOperation :: Int -> Operation
intToOperation = (!) $ fromList $ [0,1,2,3,5,6,7] `zip` [Sum .. Equal]

operationToFunc :: Operation -> ([Int] -> Int)
operationToFunc Sum     = sum
operationToFunc Product = product
operationToFunc Min     = minimum
operationToFunc Max     = maximum
operationToFunc Greater = \[n, m] -> if n >  m then 1 else 0
operationToFunc Less    = \[n, m] -> if n <  m then 1 else 0
operationToFunc Equal   = \[n, m] -> if n == m then 1 else 0

{- Parsing -}
packet :: Parser Packet
packet = do
    packetVersion <- binaryNumber 3
    packetType    <- binaryNumber 3
    case packetType of
        4 -> LiteralValue packetVersion <$> literalValuePayload
        n -> Operator packetVersion (intToOperation n) <$> operatorPayload

binaryNumber :: Int -> Parser Int
binaryNumber n = binaryToInt <$> takeBits n

takeBits :: Int -> Parser Binary
takeBits n = count n $ oneOf "01"

literalValuePayload :: Parser Int
literalValuePayload = binaryToInt <$> literalValueGroups
    where literalValueGroups = do
            (b:bs) <- takeBits 5
            if b == '0' then pure bs else (bs ++) <$> literalValueGroups

operatorPayload :: Parser [Packet]
operatorPayload = do
    lengthType <- binaryNumber 1
    case lengthType of
        1 -> binaryNumber 11 >>= flip count packet
        0 -> binaryNumber 15 >>= parseBitsWith (many1 packet)

parseBitsWith :: Parser a -> Int -> Parser a
parseBitsWith p n = fromRight undefined . P.parse p "" <$> takeBits n

{- Actual solutions -}
versionSum :: Packet -> Int
versionSum (LiteralValue v _) = v
versionSum (Operator v _ ps)  = v + sum (map versionSum ps)

eval :: Packet -> Int
eval (LiteralValue _ n) = n
eval (Operator _ op ps) = (operationToFunc op) $ map eval ps

partA :: Input -> Output
partA = versionSum

partB :: Input -> Output
partB = eval

day16 :: Day
day16 = Day 16 parse partA partB