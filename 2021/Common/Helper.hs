
module Common.Helper where

countIf :: [Bool] -> Int
countIf = foldr (\b x -> if b then x + 1 else x) 0

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn v s = case dropWhile (==v) s of
                [] -> []
                s' -> w : splitOn v s'' where (w, s'') = break (==v) s'


dedupe :: Eq a => [a] -> [a]
dedupe [] = []
dedupe (x:xs) = x : _dd x xs
    where
        _dd _ [] = []
        _dd y' (y:ys) = if y' /= y 
                        then y : _dd y ys
                        else _dd y' ys