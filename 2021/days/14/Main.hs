import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Set as S
import System.Environment

import Data.Sparse.SpMatrix
import Data.Sparse.SpVector
import Numeric.LinearAlgebra.Sparse
import Numeric.LinearAlgebra.Class

{- Day 14 - Polymers

You can get away with expanding out the strings for part 1, but by iteration 40,
the polymer sequence requires over 3GB of memory to store.

It turns out that we don't need to track that much information across iterations:

First, we only need to track the **count** of each possible element pair, rather
than track all of the pairs in a sequence. Because a particular element-pair will
always expand to the same two pairs, we can easily map a current pair count to
its "expanded" pair count using the expansion rules. 

Second, for each iteration, we know how many of each letter will be created when
the next expansion is performed. So, for every iteration, we can keep a running total
of letters given the running pair counts.

-}

type Rules = M.Map String Char

parseRule :: String -> (String, Char)
parseRule s = (takeWhile (/=' ') s, last s)

parse :: String -> (String, Rules)
parse s = let
    ls = lines s
    (f, s') = break null ls
    in (head f, M.fromList $ (map parseRule . tail) s')

apply :: Rules -> String -> String
apply rs (s1:s2:ss) = case M.lookup [s1, s2] rs of
    Just c -> s1 : c : apply rs (s2:ss)
    Nothing -> s1 : apply rs (s2 : ss)
apply _ xs = xs

applyN :: Int -> Rules -> String -> String
applyN n rs value | n > 0 = applyN (n-1) rs (apply rs value)
                  | otherwise = value

part1diff :: String -> Int
part1diff s = let
    ns = sort s
    cts = _count 1 (head ns) (tail ns)
    in maximum cts - minimum cts
    where
        _count x _ [] = [x]
        _count x curr (c:cs) | curr == c = _count (x + 1) curr cs
                             | otherwise = x : _count 1 c cs

apply2 :: Rules -> (M.Map String Integer, M.Map Char Integer) -> (M.Map String Integer, M.Map Char Integer)
apply2 rs (pairCount, charCount) = (pairCount', charCount')
    where
        charCount' = foldr step charCount (M.assocs pairCount)
        step (pair, count) cc = case M.lookup pair rs of 
            Just c' -> M.insertWith (+) c' count cc
            Nothing -> cc
        newPairs = (concat . mapMaybe step') (M.assocs pairCount)
        step' (pair, count) = let c1 = head pair
                                  c2 = last pair
                              in case M.lookup pair rs of
                                  Just c' -> Just [([c1,c'], count), ([c',c2], count)]
                                  Nothing -> Nothing
        pairCount' = foldr step'' M.empty  newPairs
        step'' (pair, count) m = M.insertWith (+) pair count m

apInit :: String -> (M.Map String Integer, M.Map Char Integer)
apInit [] = (M.empty, M.empty)
apInit [c] = (M.empty, M.singleton c 1)
apInit (c1:c2:ss) = let
    (pairs, chrs) = apInit (c2:ss)
    in (M.insertWith (+) [c1, c2] 1 pairs, M.insertWith (+) c1 1 chrs)

main :: IO ()
main = do
    contents <- getArgs >>= readFile . head

    let (template, rules) = parse contents
    
    let init = apInit template
    print $ init

    let (_, counts) = iterate (apply2 rules) init !! 40

    print $ (maximum . M.elems) counts - (minimum . M.elems) counts
