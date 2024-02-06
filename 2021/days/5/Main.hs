
{-# LANGUAGE TupleSections #-}
import qualified Data.Map as Map
import Data.List
import Data.Ratio
import System.Environment

import Control.Monad.Trans.State
import Control.Applicative
import Control.Monad
import Common.Parse

{- Day 5: Line Segments

The problem is counting the number of intersections of a set of line segments.
The line segments are discrete; they have finitely many cells which make them up.
To count the number of intersections, we can expand each line segment into its housing
cells, then aggregate each cell coordinate and count the number of cells with 2 or more
instances.
-}

type Coord = Ratio Int
data Line = Line { x1 :: Coord, y1 :: Coord, x2 :: Coord, y2 :: Coord}

instance Show Line where
    show (Line x1 y1 x2 y2) = "Line[(" ++ showCoord x1 ++ ", " ++ showCoord y1 ++ "), (" ++ showCoord x2 ++ ", " ++ showCoord y2 ++ ")]"

showCoord :: Coord -> String
showCoord r | denominator r == 1 = show $ numerator r
            | otherwise = show (numerator r) ++ "/" ++ show (denominator r)

ri :: String -> Int
ri = read

parseLine :: String -> Line
parseLine = evalState $ liftM4 Line
    ((%1) . ri <$> delim ',')
    ((%1) . ri <$> delim ' ')
    ((%1) . ri <$> (trimN 3 >> delim ','))
    ((%1) . ri <$> get)

horizontal :: Line -> Bool
horizontal (Line _ y1 _ y2) = y1 == y2

vertical :: Line -> Bool
vertical (Line x1 _ x2 _) = x1 == x2

inRange :: Ord a => a -> a -> a -> Bool
inRange x y = let
    (lo, hi) = if x <= y then (x, y) else (y, x)
    in liftA2 (&&) (>=lo) (<=hi)

axisAligned :: Line -> Bool
axisAligned = liftA2 (||) horizontal vertical


orderBy :: Ord a => (b -> a) -> (b, b) -> (b, b)
orderBy f (x, y) = if f x <= f y then (x, y) else (y, x)

order :: Ord a => (a, a) -> (a, a)
order = orderBy id


-- This is overly generic for the problem statement. It applies to
-- **every** possible line, while the problem space is only for
-- lines that are horizontal, vertical or slope=1.
segmentPoints :: Line -> [(Coord, Coord)]
segmentPoints l@(Line x1 y1 x2 y2) = if vertical l
    then let (lo, hi) = order (y1, y2) in map (x1,) [lo..hi]
    else unfoldr step (xlo, ylo)
        where
            m = (y2 - y1) / (x2 - x1)
            ((xlo, ylo), (xhi, yhi)) = orderBy fst ((x1, y1), (x2, y2))
            step (xc, yc) = if xc > xhi
                then Nothing
                else Just ((xc, yc), (xc + 1, yc + m))

countVents :: [Line] -> Map.Map (Coord, Coord) Int
countVents ls = let
    segments = concatMap segmentPoints ls
    in foldl' step Map.empty segments
    where step acc pt = Map.insertWith (+) pt 1 acc


main :: IO ()
main = do
    lines <- map parseLine . lines <$> (getArgs >>= readFile . head)

    print $ length . Map.filter (>=2) $ countVents lines
