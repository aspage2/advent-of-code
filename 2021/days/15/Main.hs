import Data.Maybe
import System.Environment
import qualified Data.Map as M
import qualified Data.HashSet as HS
import Data.Graph.AStar

type Vertex = (Int, Int)
type Costs = M.Map Vertex Int

{- Day 15 - It's just astar

We're looking for the optimal path from (0, 0) to (n, m) on the grid,
which is literally just A* search.

taxi-cab distance from the end is a good heuristic to use, as it is
admissable (it never overestimates the actual cost to reach the end).
For admissable heuristic functions, A* will give us an optimal path.

For part 2, Since the cost of a cell is a function of which cell it
is in the 5x5 monster-grid, we only need to keep track of the first
cell (the puzzle input) and define a map accordingly.

TODO: Don't be a lazy arse; try to implement A* yourself

-}

neighbors :: (Int, Int) -> Vertex -> [Vertex]
neighbors (numrow, numcol) (r, c) = filter cond candidates
    where
        cond (r', c') = 0 <= r' && r' < numrow && 0 <= c' && c' < numcol 
        candidates = [(r+1,c), (r-1,c), (r,c+1), (r,c-1)]

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

parse :: String -> ((Int, Int), Costs)
parse s = ((height, width), foldr step M.empty (zip [0..] concatd))
    where
        ls = lines s
        width = length . head $ ls
        height = length ls
        concatd = concat ls
        step (i, cost) costs = let 
                        cell' = (i `div` width, i `mod` width)
                        in M.insert cell' (c2i cost) costs

adj :: (Int, Int) -> Vertex -> HS.HashSet Vertex
adj bounds = HS.fromList . neighbors bounds

h :: (Int, Int) -> Vertex -> Int
h (nr, nc) (r, c) = nr - r + nc - c - 2

totalRisk :: Costs -> [Vertex] -> Int
totalRisk cs = sum . mapMaybe (`M.lookup` cs)

totalRisk2 :: (Int, Int) -> Costs -> [Vertex] -> Int
totalRisk2 (nr, nc) cs = let 
    cl (r, c) = let c' = (r `div` nr) + (c `div` nc) + fromJust (M.lookup (r `mod` nr, c `mod` nc) cs)
                in if c' > 9 then c' - 9 else c'
    in sum . map cl


part1Search :: (Int, Int) -> M.Map Vertex Int -> [Vertex]
part1Search bounds@(nr, nc) costs = fromJust $ aStar 
        (adj bounds) 
        (\_ v -> fromJust $ M.lookup v costs)
        (h bounds)
        (==(nr - 1, nc - 1))
        (0, 0)

part2Search :: (Int, Int) -> M.Map Vertex Int -> [Vertex]
part2Search bounds@(nr, nc) costs = let
    nr' = nr * 5
    nc' = nc * 5
    bounds' = (nr', nc')
    cl _ (r, c) = let cost = (r `div` nr) + (c `div` nc) + fromJust (M.lookup (r `mod` nr, c `mod` nc) costs)
                  in if cost > 9 then cost - 9 else cost

    in fromJust $ aStar
        (adj bounds')
        cl
        (h bounds')
        (==(nr' - 1, nc' - 1))
        (0, 0)

main = do
    contents <- getArgs >>= readFile . head

    let (bounds, costs) = parse contents

    print $ totalRisk2 bounds costs $ part2Search bounds costs