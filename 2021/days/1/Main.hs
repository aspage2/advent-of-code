import System.Environment

depths :: String -> [Int]
depths = map (read :: String -> Int) . lines

{-
  sliding-window sum of 3 adjacent elements in a list

  [1, 2, 3, 4, 5, 6]
   A  A  A          = 6
      B  B  B       = 9
         C  C  C    = 12
            D  D  D = 15

  So the value of (agg3 [1..6]) is [6, 9, 12, 15]
-}
agg3 :: [Int] -> [Int]
agg3 [] = []
agg3 [_] = []
agg3 [_, _] = []
agg3 (x:y:z:xs) = (x + y + z) : agg3 (y:z:xs)

{-
  Difference between adjacent elements of a list

  diffs [a, b, c, d] === [a - b, b - c, c - d]
-}
diffs :: [Int] -> [Int]
diffs = zipWith (flip (-)) <*> tail -- wow cool applicative functor bro

{- 
  numIncreased is the number of pairs of adjacent ints where the right
  int is greater than the left one.
-}
numIncreased :: [Int] -> Int
numIncreased v = let dfs = diffs v in sum (map pred dfs)
    where pred x 
            | x <= 0 = 0
            | otherwise = 1


main = do
    payload <- getArgs >>= readFile . head

    print $ (numIncreased . agg3 . depths) payload
    