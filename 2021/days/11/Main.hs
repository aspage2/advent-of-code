import Debug.Trace
import qualified Data.Map as M
import System.Environment
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.List as L

type Grid = A.Array (Int, Int) Int

{- Day 11: Flashing octopi

1. For a single "step" of the octopus grid, start by incrementing
the grid values by 1. If all values are 9 or below, we are done.
otherwise:

2. collect the cells which are 10 or more and mark them
"flashed". 

3. Increment all of the non-flashed neighbor cells by however
many of its neighbors flashed.

4. repeat from step 2, still tracking which cells have already flashed.

For part 2, we are guaranteed that any starting grid will sync up in a
finite amount of time, as synced octopi never de-sync and de-sync'd
octopi will eventually sync. Thus, we can just continue simulating the
grid until we find a step where all octopi flash.

-}

gridString :: Grid -> String
gridString g = L.intercalate "\n" $ _ic str
    where str = concatMap show g
          _ic [] = []
          _ic str = let (f, s) = splitAt 10 str in f : _ic s

c2i :: Char -> Int
c2i '0' = 0
c2i '1' = 1
c2i '2' = 2
c2i '3' = 3
c2i '4' = 4
c2i '5' = 5
c2i '6' = 6
c2i '7' = 7
c2i '8' = 8
c2i '9' = 9
c2i _ = undefined

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (r, c) = let r' = r - 1
                       c' = c - 1
                   in [(i, j) | i <- [r'..(r' + 2)], j <- [c'..(c' + 2)], i /= j]

parseGrid :: String -> Grid
parseGrid s =
    let ints = map c2i (concat $ lines s)
    in A.array
        ((0,0), (9, 9)) $
        zip [(i `div` 10, i `mod` 10) | i <- [0..99]] ints

step :: Grid -> (Grid, Int)
step g = let g2 = fmap (+1) g in _fl g2 S.empty
    where _fl g' vs =
            let flashers = map fst $ filter (\(i, v) -> not (S.member i vs) && v > 9) (A.assocs g')
                l = length flashers
            in if l == 0 then (g', l) else let
                vs' = S.unions (vs : map S.singleton flashers)
                ns = filter (not . (`S.member` vs')) (concatMap neighbors flashers)
                nc = foldr (\v m -> M.insertWith (+) v 1 m) M.empty ns :: M.Map (Int, Int) Int
                g'' = g' A.// M.toList (M.mapWithKey (\k v -> g' A.! k + v) nc) A.// map (\k -> (k,0)) flashers
                (g''', l') = _fl g'' vs'
                in (g''', l + l')

stepN :: Int -> Grid -> (Grid, Int)
stepN 0 g = (g, 0)
stepN n g = let (g', l) = step g; (g'', l') = stepN (n - 1) g' in (g'', l' + l)

findFullSync :: Grid -> Int
findFullSync g = _fs g 1  where _fs g n = let (g', l) = step g in if l == 100 then n else _fs g' (n + 1) 

main = do
    g <- parseGrid <$> (getArgs >>= readFile . head)

    let (_, l) = stepN 100 g

    print $ l
    print $ findFullSync g
