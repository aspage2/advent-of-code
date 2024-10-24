module Day1 where

import Data.List (foldl')

part1 :: String -> Int
part1 = foldl' f 0 
    where f acc c | c == '(' = acc + 1
                  | c == ')' = acc - 1
                  | otherwise = acc

part2 :: String -> Integer
part2 = fst . foldl' f (0, 0)
    where f (rnd, flr) c | flr < 0 = (rnd, flr)
                         | c == '(' = (rnd + 1, flr + 1)
                         | c == ')' = (rnd + 1, flr - 1)
                         | otherwise = (rnd, flr)


dayMain :: String -> IO ()
dayMain loc = do
    input <- readFile loc
    print $ part1 input
    print $ part2 input

