import qualified Control.Monad.Trans.State as St
import Data.List
import Common.Parse
import Data.Maybe
import Data.Bifunctor
import System.Environment
import qualified Data.Array as A
import qualified Data.Map as M

{- Day 20 - Imaging

It's Conway's game of life where the alive/dead rules are the image
enhancement algorithm instead of the actual rules.

There is an extra issue in this case where the infinite grid can
"flash" after each turn. Namely, for algorithm String R[0..511] where
R[0] = '#' and R[1] = '.', if you get far out enough, all cells will
be the same as their neighbors, so they will all either turn on together
or turn off together after each turn.

-}

data Status = Alive | Dead deriving (Eq, Show)
type Rules = A.Array Int Status
type Cell = (Integer, Integer)
data Bounds = Bounds { rmin :: Integer, rmax :: Integer, cmin :: Integer, cmax :: Integer } deriving (Show)
data Grid = Grid {
    -- Depending on your input ruleset, the cells past the focus-boundary (the ones with either
    -- all alive or all dead neighbors) can "flash" alive and dead. If all cells are dead
    -- (the number is 0) and Rules[0] == Alive, the infinite grid is set to "alive".
    -- In the same vein, if all cells are alive (the number is 511) and Rules[511] == Dead,
    -- the infinite grid is set to "Dead".
    extStatus :: Status,
    bounds :: Bounds,
    cells :: M.Map Cell Status
}

showGrid :: Grid -> String
showGrid g = let
    (Bounds rmn rmx cmn cmx) = bounds g
    numCol = fromIntegral $ cmx - cmn  + 1:: Int
    cs = [(r, c) | r <- [rmn..rmx], c <- [cmn..cmx]]
    m = cells g
    chrs = map (\cl -> if Alive == fromJust (M.lookup cl m) then '#' else '.') cs
    in show (bounds g) ++ "\n" ++ intercalate "\n" (toRows chrs numCol)
    where 
        toRows [] nc = []
        toRows ss nc = let (s,ss') = splitAt nc ss in s : toRows ss' nc

instance Show Grid where
    show = showGrid

takeStatus :: Char -> Status
takeStatus '#' = Alive
takeStatus '.' = Dead
takeStatus _ = undefined 

other :: Status -> Status
other Alive = Dead
other Dead = Alive

contains :: Bounds -> Cell -> Bool
contains (Bounds rmn rmx cmn cmx) (r, c) = 
    rmn <= r && r <= rmx && cmn <= c && c <= cmx

get :: Grid -> Cell -> Status
get g c = let b = bounds g in if contains b c 
    then fromJust $ M.lookup c (cells g)
    else extStatus g

getBlock :: Grid -> Cell -> [Status]
getBlock g (r, c) = [get g (r', c') |  r' <- [(r-1)..(r+1)], c' <- [(c-1)..(c+1)]]

toInt :: [Status] -> Int
toInt ss = foldr step 0 (zip [0..] ss)
    where 
        l = length ss
        step (i, Alive) = (+(2^(l - i - 1)))
        step (i, Dead) = id

takeRules :: String -> Rules
takeRules = A.array (0, 511) . zipWith (curry $ second takeStatus) [0..]

takeGrid :: String -> Grid
takeGrid s = let
    ls = lines s 
    numRow = fromIntegral $ length ls :: Integer
    numCol = fromIntegral $ length (head ls) :: Integer
    ss = map takeStatus (concat ls)
    
    in Grid {
        extStatus = Dead,
        cells = M.fromList $ zip [(r, c) | r <- [0..(numRow-1)], c <- [0..(numCol-1)]] ss,
        bounds = Bounds {rmin = 0, rmax = numRow - 1, cmin = 0, cmax = numCol - 1}
      }

parse :: String -> (Rules, Grid)
parse = St.evalState $ do
    rules <- takeRules <$> delim '\n'
    delim '\n'
    grid <- takeGrid <$> St.get
    return (rules, grid)

nextExt :: Rules -> Status -> Status
nextExt rs st = case st of
    Dead -> rs A.! 0
    Alive -> rs A.! 511

nextGrid :: Rules -> Grid -> Grid
nextGrid rules g = let
    (Bounds rmn rmx cmn cmx) = bounds g
    cs = [(r, c) | r <- [(rmn - 1)..(rmx + 1)], c <- [(cmn - 1)..(cmx + 1)]]
    m' = M.fromList $ map step cs
    in g {
        extStatus = nextExt rules (extStatus g),
        bounds = Bounds (rmn - 1) (rmx + 1) (cmn - 1) (cmx + 1),
        cells = m'
    }
    where 
        step cell = (cell, rules A.! toInt (getBlock g cell))

numAlive :: Grid -> Int
numAlive g = let
    cs = cells g
    in length . filter (==Alive) $ M.elems cs

simulate :: Rules -> Grid -> Int -> Grid
simulate r g n = last . take (n + 1) $ iterate (nextGrid r) g

main :: IO ()
main = do
    print "Hello, world"

    contents <- getArgs >>= readFile . head

    let (r, init) = parse contents

    print $ numAlive (simulate r init 50)

