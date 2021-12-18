module Days.Day18 ( day18 ) where

import AOC.Day ( Day(..) )
import Data.Either ( fromRight )
import Text.Parsec ( Parsec, (<|>), char, digit, many1 )
import qualified Text.Parsec as P ( parse )

data Tree   a = Leaf a | Branch (Tree a) (Tree a)
data Change a = Changed a | Same a

type SnailNumber = Tree Int
type Input       = [SnailNumber]
type Output      = Int

{- Tree parsing -}
parse :: String -> Input
parse = map parseLine . lines
    where parseLine = fromRight undefined . P.parse (tree number) ""
          number = read <$> many1 digit 

tree :: Read a => Parsec String () a -> Parsec String () (Tree a)
tree leaf = Branch <$ char '[' <*> subtree <* char ',' <*> subtree <* char ']'
    where subtree = tree leaf <|> (Leaf <$> leaf)

{- Tree operations -}
mapLeftmost :: (a -> a) -> Tree a -> Tree a
mapLeftmost f (Leaf n)     = Leaf   (f n)
mapLeftmost f (Branch l r) = Branch (mapLeftmost f l) r

mapRightmost :: (a -> a) -> Tree a -> Tree a
mapRightmost f (Leaf n)     = Leaf   (f n)
mapRightmost f (Branch l r) = Branch l (mapRightmost f r)

{- Operations on snail numbers -}
explode :: SnailNumber -> Change SnailNumber
explode = (\(a,_,_) -> a) . explode' 0
    where explode' _ (Leaf n)                     = (Same    $ Leaf n, 0, 0)
          explode' 4 (Branch (Leaf lv) (Leaf rv)) = (Changed $ Leaf 0, lv, rv)
          explode' d t@(Branch l r)
              | (Changed l', lv, rv) <- explode' (d+1) l = (Changed $ Branch l' (mapLeftmost (+rv) r) , lv, 0)
              | (Changed r', lv, rv) <- explode' (d+1) r = (Changed $ Branch (mapRightmost (+lv) l) r', 0, rv)
              | otherwise                                = (Same t, 0, 0)

split :: SnailNumber -> Change SnailNumber
split (Branch l r)
    | (Changed l') <- split l = Changed $ Branch l' r
    | (Changed r') <- split r = Changed $ Branch l  r'
    | otherwise               = Same $ Branch l r
split (Leaf n)
    | n < 10    = Same $ Leaf n
    | otherwise = Changed $ Branch (Leaf $ floor   $ fromIntegral n / 2)
                                   (Leaf $ ceiling $ fromIntegral n / 2)

(<+>) :: SnailNumber -> SnailNumber -> SnailNumber
n <+> m = reduce $ Branch n m
    where reduce n
            | (Changed n') <- explode n = reduce n'
            | (Changed n') <- split n   = reduce n'
            | otherwise                 = n

magnitude :: SnailNumber -> Int
magnitude (Branch l r) = (3 * magnitude l) + (2 * magnitude r)
magnitude (Leaf   n)   = n

partA :: Input -> Output
partA = magnitude . foldl1 (<+>)

partB :: Input -> Output
partB = maximum . map magnitude . sums
    where sums ns = [n1 <+> n2 | n1 <- ns, n2 <- ns]

day18 :: Day
day18 = Day 18 parse partA partB