module Day11 where

import Data.Char (ord, chr)
import Data.List (nub)

_increment :: String -> (Bool, String)
_increment "" = (True, "")
_increment (x:xs) = let 
        (carried, xs') = _increment xs
        carried' = carried && (x >= 'z')
        x' = if carried then if x == 'z' then 'a' else chr (ord x + 1) else x
        in (carried', x':xs')

increment :: String -> String
increment s = let (c, s') = _increment s in if c then 'a':s' else s'

noAmbiguousChars :: String -> Bool
noAmbiguousChars = all (`notElem` "iol")

triplets :: [a] -> [(a, a, a)]
triplets xs = zip3 xs (drop 1 xs) (drop 2 xs)

pairs :: [a] -> [(a, a)]
pairs xs = zip xs (drop 1 xs)

hasAStraight :: String -> Bool
hasAStraight = any p . triplets
    where p (a, b, c) = ord a == ord b - 1 && ord b == ord c - 1

moreThanOnePair :: String -> Bool
moreThanOnePair s = length uniqPairs >= 2
    where uniqPairs = nub $ filter (uncurry (==)) $ pairs s

valid :: String -> Bool
valid s = hasAStraight s && moreThanOnePair s && noAmbiguousChars s

dayMain :: String -> IO ()
dayMain _ = do
    let em = iterate increment "hepxcrrq"

    print $ take 2 $ filter valid em
