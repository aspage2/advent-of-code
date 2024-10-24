module Day3 where

import Data.List (foldl')

import qualified Data.Map as M

type Grid = M.Map (Int, Int) Int

makeMove :: (Int, Int) -> Char -> (Int, Int)
makeMove (r, c) d = 
  case d of 
    'v' -> (r - 1, c)
    '^' -> (r + 1, c)
    '<' -> (r, c - 1)
    '>' -> (r, c + 1)
    _ -> (r, c)

record :: Grid -> (Int, Int) -> Grid
record g c = M.insertWith (+) c 1 g

initGrid :: Int -> Grid
initGrid = M.singleton (0, 0)

oneSanta :: [Char] -> Grid
oneSanta = fst . foldl' f (initGrid 1, (0, 0))
    where f (g, p) d = let newC = makeMove p d in (record g newC, newC)

twoSantas :: [Char] -> Grid
twoSantas = _ts (initGrid 2) (0, 0) (0, 0) 
    where _ts g s rs ds =
              case ds of
                [] -> g
                ds':dr:rest -> let
                    newS = makeMove s ds'
                    newRS = makeMove rs dr
                    in _ts (record (record g newS) newRS) newS newRS rest
                [ds'] -> record g (makeMove s ds')

dayMain :: String -> IO ()
dayMain fname = do
    d <- readFile fname
    print $ M.size $ oneSanta d
    print $ M.size $ twoSantas d


