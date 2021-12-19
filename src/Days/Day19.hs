module Days.Day19 ( day19 ) where

import AOC.Day         ( Day(..) )
import Data.Either     ( Either(..), partitionEithers )
import Data.List.Split ( splitOn )
import Data.Set        ( Set, fromList, intersection, toList, unions )
import qualified Data.Set as S ( map )

data Point    = P Int Int Int deriving (Eq, Ord)
type Beacons  = Set Point
type Scanners = [Point]
type Shift    = Point 
type Input    = [Beacons]
type Output   = Int

instance Num Point where
    (P a b c) + (P x y z) = P (a+x) (b+y) (c+z)
    (P a b c) - (P x y z) = P (a-x) (b-y) (c-z)
    (P a b c) * (P x y z) = P (a*x) (b*y) (c*z)
    abs p         = error "not implemented"
    signum p      = error "not implemented"
    fromInteger p = error "not implemented"

parse :: String -> Input
parse = map (fromList . parsePoints) . splitOn "\n\n" . init
    where parsePoints  = map parsePoint . drop 1 . splitOn "\n"
          parsePoint p = let (x, y, z) = read $ "(" <> p <> ")" in P x y z

allRotations :: Set Point -> [Set Point]
allRotations ps = [ S.map (f . g . h . i) ps
                  | f <- [id, flipFirst]
                  , g <- [id, flipSecond]
                  , h <- [id, flipThird]
                  , i <- [swap123, swap132, swap213, swap231, swap312, swap321] ]
    where flipFirst  (P a b c) = P (-a) b c
          flipSecond (P a b c) = P a (-b) c
          flipThird  (P a b c) = P a b (-c)
          swap123 (P a b c) = P a b c 
          swap132 (P a b c) = P a c b
          swap213 (P a b c) = P b a c
          swap231 (P a b c) = P b c a
          swap312 (P a b c) = P c a b
          swap321 (P a b c) = P c b a

tryToAlign :: Beacons -> Beacons -> Either Beacons (Beacons, Shift)
tryToAlign fixed ps = if null shifts then Left ps else Right $ head shifts
    where shifts = [ (shifted, shift)
                   | fixedPoint <- toList fixed
                   , rotatedPs  <- allRotations ps
                   , p          <- toList rotatedPs
                   , let shift = fixedPoint - p
                   , let shifted = S.map (+ shift) rotatedPs
                   , let intersectionSize = length (fixed `intersection` shifted)
                   , intersectionSize >= 12 ]

alignFirst :: (Beacons, Scanners, [Beacons]) -> (Beacons, Scanners, [Beacons])
alignFirst (aligned, shifts, notAligned) = (aligned', shifts', notAligned')
    where (notAligned', res) = partitionEithers $ map (tryToAlign aligned) notAligned
          aligned'           = unions $ aligned : (map fst res)
          shifts'            = shifts ++ (map snd res)

alignAll :: [Beacons] -> (Beacons, Scanners)
alignAll (ps:pss) = dropThird . head . filter (null . third) $ iterate alignFirst (ps, [], pss)
    where dropThird (x, y, _) = (x, y)
          third     (_, _, x) = x

partA :: Input -> Output
partA = length . fst . alignAll

partB :: Input -> Output
partB = maximum . allDistances . snd . alignAll
    where allDistances ps = [norm $ p1 - p2 | p1 <- ps, p2 <- ps, p1 /= p2]
          norm (P a b c)  = abs a + abs b + abs c   

day19 :: Day
day19 = Day 19 parse partA partB