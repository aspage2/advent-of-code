module Day5 where

import qualified Data.Map as M
import Control.Applicative (liftA3, Applicative (liftA2))
import Data.List (foldl')

countPred :: (a -> Bool) -> [a] -> Int 
countPred p = foldl' (\a c -> if p c then a + 1 else a) 0

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs x@(_:xs) = zip x xs

triples :: [a] -> [(a, a, a)]
triples [] = []
triples [_] = []
triples [_, _] = []
triples xs = let
    _:rest = xs
    _:rest' = rest
    in zip3 xs rest rest'

-- Part 1
countVowels :: String -> Int
countVowels = countPred isVowel 
    where isVowel = flip elem ['a', 'e', 'i', 'o', 'u']

containsOneDouble :: String -> Bool
containsOneDouble = any (uncurry (==)) . pairs

doesntContainBadPhrase :: String -> Bool
doesntContainBadPhrase = 
    all (\(a, b) -> [a,b] `notElem` ["ab", "cd", "pq", "xy"]) 
    . pairs

part1Nice :: String -> Bool
part1Nice = liftA3 and3
              doesntContainBadPhrase 
              containsOneDouble 
              ((>=3) . countVowels)
    where and3 True True True = True
          and3 _ _ _ = False

-- Part 2
containsSpecialTriple :: String -> Bool
containsSpecialTriple = any (\(a, _, c) -> a == c) . triples

containsNonOverlappingPairs :: String -> Bool
containsNonOverlappingPairs [] = False
containsNonOverlappingPairs [_] = False
containsNonOverlappingPairs xs = any (>=2) (M.elems (snd res)) where
    res = foldl' f (Nothing, M.empty :: M.Map (Char, Char) Int) $ pairs xs
    f (Nothing, m) p = new p m
    f (Just p, m) p' | p == p'   = (Nothing, m)
                     | otherwise = new p' m
    new p m = (Just p, M.insertWith (+) p 1 m)


part2Nice :: String -> Bool
part2Nice = liftA2 (&&) containsSpecialTriple containsNonOverlappingPairs

--------------------------------------------------------

dayMain :: String -> IO ()
dayMain fname = do
    d <- lines <$> readFile fname
    print $ countPred part1Nice d
    print $ countPred part2Nice d

