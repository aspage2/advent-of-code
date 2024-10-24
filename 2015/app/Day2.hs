module Day2 where

import Control.Applicative (liftA2)
import Control.Monad (liftM3)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Text.Read (readMaybe)

data Present = Present !Int !Int !Int

minFace :: (Int -> Int -> Int) -> Present -> Int
minFace predicate (Present l w h) =
    predicate l w `min` predicate l h `min` predicate w h

perim :: Int -> Int -> Int
perim b h = 2*b + 2*h
    
surfaceArea :: Present -> Int
surfaceArea (Present l w h) = 2*l*w + 2*l*h + 2*h*w

volume :: Present -> Int
volume (Present l w h) = l*w*h

paperNeeded :: Present -> Int
paperNeeded = liftA2 (+) surfaceArea (minFace (*))

ribbonNeeded :: Present -> Int
ribbonNeeded = liftA2 (+) volume (minFace perim)

parsePresents :: String -> [Present]
parsePresents = mapMaybe parse . lines
    where parse line = let
                (l, rest) = takeUntil 'x' line
                (w, h) = takeUntil 'x' rest 
                in liftM3 Present (readMaybe l) (readMaybe w) (readMaybe h)
          takeUntil c l = case l of
            [] -> ([], [])
            x:xs | x == c -> ([], xs)
                 | otherwise -> let (p, r) = takeUntil c xs in (x:p, r)

pureMain :: String -> (Int, Int)
pureMain = foldl' f (0, 0) . parsePresents
    where f (l, r) p = (l + paperNeeded p, r + ribbonNeeded p)

dayMain :: String -> IO ()
dayMain fname = do
    (part1, part2) <- pureMain <$> readFile fname
    print part1
    print part2

