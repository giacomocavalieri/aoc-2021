module Days.Day4 ( day4 ) where

import AOC.Day                    ( Day(..) )
import Control.Monad              ( void )
import Data.List                  ( find, transpose )
import Data.Maybe                 ( fromMaybe )
import Text.Parsec hiding         ( parse )
import qualified Text.Parsec as P ( parse )

type Parser = Parsec String ()

data BingoNumber = NotDrawn Int | Drawn Int deriving (Eq)
type BingoBoard = [[BingoNumber]]
type Input = ([Int], [BingoBoard])
type Output = Int

{- Input parsing -}
parse :: String -> Input
parse s = case P.parse input "" s of
    Left  e -> error $ show e
    Right i -> i

number :: Parser Int
number = read <$> many1 digit

bingoLine :: Parser [BingoNumber]
bingoLine = do
    many $ char ' '
    bingoLine <- map NotDrawn <$> number `sepBy` (many1 $ char ' ')
    newline
    pure bingoLine

bingoBoard :: Parser BingoBoard
bingoBoard = do
    board <- count 5 bingoLine
    eof <|> void newline
    pure board

input :: Parser Input
input = do
    firstLine <- number `sepBy` (char ',') 
    many1 newline
    boards <- many1 bingoBoard
    pure (firstLine, boards)

{- Challenge solution -}
drawNumber :: Int -> BingoBoard -> BingoBoard
drawNumber n board = map (map switchToDrawn) board
    where switchToDrawn (NotDrawn x) = if x == n then Drawn n else NotDrawn x
          switchToDrawn drawn = drawn

isDrawn :: BingoNumber -> Bool
isDrawn (Drawn _)    = True
isDrawn (NotDrawn _) = False

isWinning :: BingoBoard -> Bool
isWinning board = any isRowComplete board || any isRowComplete (transpose board)
    where isRowComplete = all isDrawn

-- Keep drawing numbers until one (or more) winning board is found, returns:
-- * the last extracted number
-- * the numbers that were not extracted
-- * the first of all the winning boards
-- * the remaining boards updated to reflect the extractions of the numbers
drawUntilOneWins :: [Int] -> [BingoBoard] -> (Int, [Int], BingoBoard, [BingoBoard])
drawUntilOneWins (n:ns) boards = case winningBoards of
    (board:_) -> (n, ns, board, losingBoards)
    []        -> drawUntilOneWins ns boards'
    where boards'       = map (drawNumber n) boards
          winningBoards = filter isWinning boards'
          losingBoards  = filter (not . flip elem winningBoards) boards'

firstWinningBoard :: [Int] -> [BingoBoard] -> (Int, BingoBoard)
firstWinningBoard ns boards = (n, board)
    where (n, _, board, _) = drawUntilOneWins ns boards

lastWinningBoard :: [Int] -> [BingoBoard] -> (Int, BingoBoard)
lastWinningBoard ns boards
    | null boards' = (winningNumber, winningBoard)
    | otherwise    = lastWinningBoard ns' boards'
    where (winningNumber, ns', winningBoard, boards') = drawUntilOneWins ns boards 

getResult :: (Int, BingoBoard) -> Int
getResult (n, board) = n * sumOfUndrawn
    where sumOfUndrawn = sum [n | (NotDrawn n) <- concat board]

partA :: Input -> Output
partA = getResult . uncurry firstWinningBoard

partB :: Input -> Output
partB = getResult . uncurry lastWinningBoard

day4 :: Day
day4 = Day 4 parse partA partB