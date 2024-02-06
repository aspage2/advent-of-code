import Data.Maybe
import System.Environment
import Data.List
import Data.Char (isLower)
import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.Set as S

{- Day 12 - Caves

It's a graph walk where certain nodes can be visited more than once. We
can use a normal graph data structure and a search algorithm with expansion
rules based on the rules given in the prompt.

-}

type Vertices = M.Map String Int
type Edges = A.Array (Int, Int) Bool
type Graph = (Vertices, Edges)

-- Dedupe elements of a "grouped" list, e.g. a list whose
-- like terms are adjacent:
--   grouped:     [1,1,2,2,2,3,4,4,4]
--   not grouped: [1,2,3,1,3,4]
dedupe :: Eq a => [a] -> [a]
dedupe [] = []
dedupe (x:xs) = x : _dd x xs
    where
        _dd _ [] = []
        _dd y' (y:ys) = if y' /= y 
                        then y : _dd y ys
                        else _dd y' ys

parseGraph :: String -> Graph
parseGraph s = let
    pairs = map (\l -> let (f, s) = break (=='-') l 
                       in (f, tail s)) (lines s)
    vnames = (dedupe . sort) $ concatMap (\(a, b) -> [a, b]) pairs
    vs = M.fromList (zip vnames [0..])
    d = M.size vs - 1
    arr = A.array ((0, 0), (d, d)) [((r, c), False)|r<-[0..d], c<-[0..d]]
    arr' = arr A.// concatMap (\(n1, n2) -> let i1 = fromJust $ M.lookup n1 vs
                                                i2 = fromJust $ M.lookup n2 vs
                                            in [((i1, i2), True), ((i2, i1), True)]) pairs
    in (vs, arr')

isSmall :: String -> Bool
isSmall = all isLower

invLookup :: Int -> Vertices -> String
invLookup i = fst . head . filter (\(n, i') -> i' == i) . M.assocs

neighbors :: String -> Graph -> [String]
neighbors v (vs, es) = let
    vnum = fromJust $ M.lookup v vs
    d = M.size vs - 1
    row = map (\c -> (c, es A.! (vnum,c))) [0..d]
    in map (\i -> invLookup (fst i) vs) $ filter snd row

walk :: a -> (a -> String -> Bool) -> (a -> String -> a) -> Graph -> [[String]]
walk init valid vsUpdate (vs, es) = _w "start" init 
    where
        _w "end" _ = [["end"]]
        _w node st = let
            st' = vsUpdate st node
            ns = filter (valid st') (neighbors node (vs, es))
            in concatMap (\n -> map (node:) $ _w n st') ns


part1Walk' :: Graph -> [[String]]
part1Walk' = walk S.empty (\vs n -> not (isSmall n && S.member n vs)) (flip S.insert) 

part2Walk' :: Graph -> [[String]]
part2Walk' = walk (S.empty, False)
    (\(vs, st) n -> (n /= "start") && not (isSmall n) || not st || not (S.member n vs))
    (\(vs, st) n -> (S.insert n vs, st || S.member n vs && isSmall n))

part1Walk :: Graph -> [[String]]
part1Walk (vs, es) = _w "start" S.empty
    where
        _w "end" _ = [["end"]]
        _w node visited = let
            visited' = S.insert node visited
            valid = not . ((&&) <$> isSmall <*> (`S.member` visited'))
            ns = filter valid (neighbors node (vs, es))
            in concatMap (\n -> map (node:) $ _w n visited') ns

part2Walk :: Graph -> [[String]]
part2Walk (vs, es) = _w "start" (S.empty, False)
    where
        _w "end" _ = [["end"]]
        _w node (visited, smallTwice) = let
            visited' = S.insert node visited
            smallTwice' = smallTwice || S.member node visited && isSmall node
            valid "start" = False
            valid n = not (isSmall n) || not smallTwice' || not  (S.member n visited') 
            ns = filter valid (neighbors node (vs, es))
            in concatMap (\n -> map (node:) $ _w n (visited', smallTwice')) ns

test :: [String] -> Bool
test p = let step v m' = M.insertWith (+) v 1 m'
             m = foldr step M.empty p
         in length (filter (\(k, c) -> c >= 2 && isSmall k) (M.assocs m)) > 1


main = do
    contents <- getArgs >>= readFile . head

    let g = parseGraph contents

    print $ length $part2Walk' g
